# -*- coding: utf-8 -*-
"""
Created on Mon May 27 19:59:24 2019

@author: Sam Alexander
"""

class InventoryAllocator(object):
    def __init__(self, order, stock):
        '''
        Initializes an InventoryAllocator object
                
        map  (order): the number of copies demanded for each item
        list (stock): a list of objects giving warehouse supply levels
        * e.g., [{'name': 'mr_warehouse', 'inventory': {'good_thing': 1, 'bad_thing': 2 } }, ... ]
        
        
        '''
        self.amount_ordered = order
        self.stock_list = stock
        
    def build_order(self):
        '''
        Builds the order, e.g., a list of objects giving the number of items shipped from each warehouse. 
        Will be called by self.get_order() such that getting the order only builds it if needed.
        Will be stored under self.completed_order
        
        '''
        
        # demand list. since it will be modified, we want a copy of it
        needed = self.amount_ordered.copy()
        
        # extracts the number of copies of an item at a given warehouse and reduces demand in 'needed'
        #  returns None if something goes wrong
        def stockfunction(obj, key):
            try:
                outval = obj['inventory'][key]
                outval = min(outval, needed[key])
                # don't want to subtract a negative.. handle elsewhere if results should not be negative
                if outval > 0:
                    needed[key] -= outval
            except: 
                outval = None
            return outval
        
        # build output as a dict and convert later
        out = {}
        for item in needed.keys():
            # **OPTIONAL**: do not include items if order cannot be completed fully (ENABLE FOR 'all-or-nothing' RETURNS)
#            total_stock = [ wh['inventory'].get(item) for wh in self.stock_list if wh['inventory'].get(item) is not None ]
#            if sum(total_stock) < needed.get(item):
#                continue
            
            # for each item, start at warehouse_1 and try to fill order
            #  if too few in stock, move to warehouse_2, et cetera
            for wh in test.stock_list:
                val = stockfunction(wh, item)
                if val is None:
                    continue
                # **OPTIONAL**: If supply < 0 should raise an error instead:
#                elif val < 0:
#                    raise ValueError('Supply is less than zero for {itm} in {wh}'.format(itm=repr(item),wh=repr(wh['name'])))
                elif val > 0:
                    if wh['name'] not in out.keys():
                        out[wh['name']] = {}
                    out[ wh['name'] ][item] = val
                # stop checking when demand filled
                if needed.get(item) == 0:
                    break
        self.completed_order = [{wh: out[wh]} for wh in out]
            
    
    def get_order(self):
        '''
        The getter function for the order to be placed
        '''
        try:
            return self.completed_order
        except AttributeError: 
            self.build_order()
            return self.completed_order


## TEST CASES (requires identical list orders, but order should be preserved)
#  supply at first warehouse == demand          
print('Test 1: Happy Case, exact inventory match!')
print(' -- expected: [{ owd: { apple: 1 } }]')
test = InventoryAllocator({ 'apple': 1 }, [{ 'name': 'owd', 'inventory': { 'apple': 1 } }])
if test.get_order() == [{ 'owd': { 'apple': 1 } }]:
    print('Passed: ' + str(test.get_order()))
else:
    print('FAILED: actual response = ' + str(test.get_order()))

#  supply at first warehouse exceeds demand          
print('\nTest 2: Supply in warehouse 1 exceeds order')
print(' -- expected: [{ owd: { apple: 1 } }]')
test = InventoryAllocator({ 'apple': 1 }, [{ 'name': 'owd', 'inventory': { 'apple': 2 } }])
if test.get_order() == [{ 'owd': { 'apple': 1 } }]:
    print('Passed: ' + str(test.get_order()))
else:
    print('FAILED: actual response = ' + str(test.get_order()))
        
#  insufficient supply          
print('\nTest 3: Demand and no supply (return nothing)')
print(' -- expected: []')
test = InventoryAllocator({ 'apple': 1 }, [{ 'name': 'owd', 'inventory': { 'apple': 0 } }])
if test.get_order() == []:
    print('Passed: ' + str(test.get_order()))
else:
    print('FAILED: actual response = ' + str(test.get_order()))

#  partial supply (I am assuming the 0 return was due to 0 supply, and NOT that it should be all-or-nothing )      
print('\nTest 4: Demand exceeds supply (return some)')
print(' -- expected: []')
test = InventoryAllocator({ 'apple': 2 }, [{ 'name': 'owd', 'inventory': { 'apple': 1 } }])
if test.get_order() == [{ 'owd': { 'apple': 1 } }]:
    print('Passed: ' + str(test.get_order()))
else:
    print('FAILED: actual response = ' + str(test.get_order()))

#  use multiple warehouses if needed         
print('\nTest 5: Order requires multiple warehouses')
print(' -- expected: [{ dm: { apple: 5 }}, { owd: { apple: 5 } }]')
test = InventoryAllocator({ 'apple': 10 }, [{ 'name': 'owd', 'inventory': { 'apple': 5 } }, { 'name': 'dm', 'inventory': { 'apple': 5 }}])
if test.get_order() == [{ 'owd': { 'apple': 5 }}, { 'dm': { 'apple': 5 } }]:
    print('Passed: ' + str(test.get_order()))
else:
    print('FAILED: actual response = ' + str(test.get_order()))
    
#  treat negative values as zero (assumes they're debts or something)
print('\nTest 6: Treat negative stock as zero stock (and not generate an error)')
print(' -- expected: []')
test = InventoryAllocator({ 'apple': 1}, [{ 'name': 'owd', 'inventory': { 'apple': -1} }])
if test.get_order() == []:
    print('Passed: ' + str(test.get_order()))
else:
    print('FAILED: actual response = ' + str(test.get_order()))

#  if one item can be filled and one not, send the one that can be filled 
print('\nTest 7: Ship all items which can be supplied (and do not list those that cannot)')
print(' -- expected: [{dm: {pear: 1}}]')
test = InventoryAllocator({ 'apple': 1, 'pear': 1}, [{ 'name': 'owd', 'inventory': { 'apple': -1} }, { 'name': 'dm', 'inventory': {'pear': 1} }])
if test.get_order() == [{'dm': {'pear': 1}}]:
    print('Passed: ' + str(test.get_order()))
else:
    print('FAILED: actual response = ' + str(test.get_order()))    

#  failed test case (should come back as FAILED)
print('\nTest 8: Negative in one warehouse, positive in the other (could subtract negative from demand & send too many)')
print(' -- expected: [{dm: {apple: 1}}]')
test = InventoryAllocator({ 'apple': 1}, [{ 'name': 'owd', 'inventory': { 'apple': -1} }, { 'name': 'dm', 'inventory': {'apple': 2} }])
if test.get_order() == [{'dm': {'apple': 1}}]:
    print('Passed: ' + str(test.get_order()))
else:
    print('FAILED: actual response = ' + str(test.get_order()))   

#  failed test case (should come back as FAILED)
print('\nTest 9: Value mismatch generates a \'FAILED\' message')
print(' -- expected: [{dm: {pear: 1}}]')
test = InventoryAllocator({ 'apple': 1, 'pear': 1}, [{ 'name': 'owd', 'inventory': { 'apple': 1} }, { 'name': 'dm', 'inventory': {'pear': 1} }])
if test.get_order() == [{'dm': {'pear': 1}}]:
    print('Passed: ' + str(test.get_order()))
else:
    print('FAILED: actual response = ' + str(test.get_order()))  

