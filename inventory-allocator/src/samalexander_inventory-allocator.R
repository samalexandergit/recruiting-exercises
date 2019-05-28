## helper functions
#   R doesn't really do objects, so you'd technically want to use different formats as input
#   ..but let's assume we're stuck with objects, which are loaded as character strings (R default)
#   ..which means we'll need some helper functions before we can even begin, since we're also use base R why not?
{
  # split object lists
  lssplit <- function(lst){
    # count curly braces to split array:
    #  set '{' = 1 & '}' = -1 then split where counter is 0
    ls <- gregexpr('{', lst, fixed = T)[[1]]
    rs <- gregexpr('}', lst, fixed = T)[[1]]
    
    # e.g., c( 1 = '3', 1 = '47', -1 = '46', -1 = '59') -> c( 1 = '3', -1 = '46', 1 = '47, -1 = '59')
    out <- c( setNames(rep(1, length(ls)),ls), setNames(rep(-1, length(ls)),rs))
    out <- out[order(as.integer(names(out)))]
    
    # include final char as an end regardless; ensure it isn't also a start
    #  note: these are indices of an index
    ends <- unique(which( cumsum(out) == 0), length(out))
    starts <- c(out[1], ends[-length(ends)] + 1)
    
    # take substrings using start/end values inserted back into the index stored in names(out)
    substring(lst, names(out)[starts], names(out)[ends])
  }
  
  # get value from a key ( assumes char or numeric & assumes single-value inputs )
  JSval <- function(js, key){
    # since the sample keys were not quoted, let's assume we don't have to worry about stupid things in the keys
    #  hence, take everything from the ':' to the ',' or '}' and substitute away the junk
    
    # note: 'val' will be a vector giving the start locations of requested pattern with other info as attributes
    val <- gregexpr(paste0(key,':\\s*.*?[,}]'), js)[[1]] 
    val <- gsub(paste0(key,':\\s*'),'',substr(js, val[1], val[1] + attr(val, 'match.length') - 1))
    gsub('[,}]','',val) # replaces final ',' or '}' with empty string
  }
  
  # get value from a key when it's a shallow object too
  JSobject <- function(js, key){
    # same as JSval save that it only accepts '}' as end values and doesn't remove final '}'
    #  NOTE: will fail if object has its own sub-objects
    
    name <- gregexpr(paste0(key,':\\s*.*?[}]'), js)[[1]]
    gsub(paste0(key,':\\s*'),'',substr(js, name[1], name[1] + attr(name, 'match.length') - 1))
  }
  
  # get keys (will probably mangle keys if you somehow manage to feed it one with a space)
  JSkeys <- function(js){
    # split on ','s, remove left curly braces + spaces, split on ':' and take left-sides
    # e.g., '{ apple: 1, pear: 2 }' -> '{ apple: 1' & ' pear: 2 }' -> 'apple:1' & 'pear:2}' -> 'apple' & 'pear'
    out <- strsplit(js, ',')[[1]]
    out <- gsub('[{ ]','',out)
    out <- strsplit(out, ':')
    out <- sapply(out, function(x) x[1])
    
    out
    
  }
  
}

  
# created a function instead because I didn't read 'class' somehow
orderfill <- function(ord, stk){
  # reduce order to a list of key/value pairs
  ord <- strsplit(ord, ',')[[1]]
  ord <- gsub('[{} ]', '', ord)
  ord <- strsplit(ord, ':')
  
  # reduce stock to a vector of objects-as-strings
  stk <- lssplit(stk)
  
  # get warehouse names
  nams <- sapply(stk, function(x) JSval(x, 'name'))
  
  # R's main data type is a data.frame, which is basically a SQL table
  # start with two columns: warehouse name + inventory as object-string
  stkdf <- data.frame( warehouse = nams)
  stkdf$js <- sapply(stk, function(x) JSobject(x, 'inventory'))
  
  # get unique keys from inventory sub-objects 
  keys <- unique(unlist(sapply(stkdf$js, function(x) JSkeys(x))))
  
  # for each key, add as a new column in stock data.frame
  #  i.e., populate the table with stock numbers by product by warehouse
  for(i in keys){
    # missing values will be '' which is NA when converted to integers, hence suppressWarnings(...)
    stkdf[[i]] <- suppressWarnings(as.integer(sapply(stkdf$js, function(x) JSval(x, i))))
  }
  
  # remove unwanted columns, convert NAs to 0s
  stkdf$js <- stkdf$keys <- NULL
  stkdf[is.na(stkdf)] <- 0
  
  # will need to transpose table because we built it wrong
  stkdf <- data.frame(t(stkdf[,-1]))
  names(stkdf) <- nams
  stkdf <- data.frame(item = keys, stkdf)
  
  # output data frame (eventually) containing values to be shipped by product by warehouse
  out <- data.frame(item = sapply(ord, function(x) x[1]), 
                    quantity = sapply(ord, function(x) as.integer(x[2])))
  # merge with stock data frame to get .. stock
  out <- merge(out, stkdf, by = 'item')
  
  # allowed to assume first warehouse is cheapest, and first warehouse is to the left of next, so..
  # take all stock from each warehouse until order is satisfied
  # at each iteration, reduce the number ordered by the amount filled by the current warehouse
  # do not remove negative values from number ordered
  for(i in 3:ncol(out)){
    wh <- names(out)[i]
    tmp <- pmin(out$quantity, out[,i])
    out[[ names(out)[i] ]] <- ifelse(tmp > 0, paste0(' ',out$item,': ', tmp), '')
    out$quantity <- out$quantity - pmax(0,tmp)
    
    rm(wh,tmp,i)
  }
  
  # need to format the output text to be valid
  outlst <- c()
  for(i in seq_along(nams) + 2){
    # do not include warehouse if every product is either 0 or could not be filled across warehouses
    if( all( out[, i] == '' | out$quantity != 0)) next()
    
    # do not include products which could not be filled across warehouses or aren't being shipped
    tmp <- out[,i]
    tmp[out$quantity != 0] <- '' 
    tmp <- tmp[ tmp != '' ]
    tmp <- paste0(tmp, collapse = ', ')
    
    # format --   '{ WAREHOUSE_NAME: {product_name: product_amount, ... } }'
    outlst <- c(outlst, paste0('{ ', nams[i-2], ': {', tmp, ' } }'))
    
    rm(tmp,i)
  }
  
  # format -- '[ { name: WAREHOUSE_1, inventory: {...} }, ... ]
  paste0('[', paste0(outlst, collapse = ', '), ']')
}


## Testing
{
  cat('Exact match -- should be: [{ owd: { apple: 1 } }]\n')
  tmp <- orderfill('{ apple: 1 }', '[{ name: owd, inventory: { apple: 1 } }]\n')
  if(tmp != '[{ owd: { apple: 1 } }]'){
    cat(paste0('FAILED: ', 'Test 1 - Exact Match \n  Returned: ', tmp))
  } else{
    cat(paste0('Passed: ',tmp))
  }
  
  
  cat('\n\nWarehouse is empty -- should be: []\n')
  tmp <- orderfill('{ apple: 1 }', '[{ name: owd, inventory: { apple: 0 } }]')
  if(tmp != '[]'){
    cat(paste0('FAILED: ', 'Test 2: Warehouse is Empty \n  Returned: ', tmp))
  } else{
    cat(paste0('Passed: ',tmp))
  }
  
  cat('\n\nIf warehouse can only partially fill, send back nothing -- should be []:\n')
  tmp <- orderfill('{ apple: 10 }', '[{ name: owd, inventory: { apple: 0 } }, { name: dm, inventory: { apple: 5 }}]')
  if(tmp != '[]'){
    cat(paste0('FAILED: ', 'Test 3: Partial Fill \n  Returned: ', tmp))
  } else{
    cat(paste0('Passed: ',tmp))
  }
  
  cat('\n\nIf order can be filled by splitting across warehouses, do so\n -- should be [{ owd: { apple: 5 } }, { dm: { apple: 5 } }]\n')
  tmp <- orderfill('{ apple: 10 }', '[{ name: owd, inventory: { apple: 5 } }, { name: dm, inventory: { apple: 5 }}]')
  if(tmp != '[{ owd: { apple: 5 } }, { dm: { apple: 5 } }]'){
    cat(paste0('FAILED: ', 'Test 4: Split Order Across Warehouses \n  Returned: ', tmp))
  } else{
    cat(paste0('Passed: ',tmp))
  }
  
  cat('\n\nTreat negatives as 0, assuming they owe stuff or something -- should be [{ dm: { apple: 10 } }]\n')
  tmp <- orderfill('{ apple: 10 }', '[{ name: owd, inventory: { apple: -5 } }, { name: dm, inventory: { apple: 15 }}]')
  if(tmp != '[{ dm: { apple: 10 } }]'){
    cat(paste0('FAILED: ', 'Test 5: Negatives Treated as Zero \n  Returned: ', tmp))
  } else{
    cat(paste0('Passed: ',tmp))
  }
  
  cat('\n\nIn spite of above, if one item can be filled, send it -- should be: [{ owd: { apple: 5 } }, { dm: { apple: 5 } }]\n')
  tmp <- orderfill('{ apple: 10, banana: 10 }', '[{ name: owd, inventory: { apple: 5, banana: 5 } }, { name: dm, inventory: { apple: 15 }}]') 
  if(tmp != '[{ owd: { apple: 5 } }, { dm: { apple: 5 } }]'){
    cat(paste0('FAILED: ', 'Test 6: Send Anything That Can Be Filled Fully \n  Returned: ', tmp))
  } else{
    cat(paste0('Passed: ',tmp))
  }
}

