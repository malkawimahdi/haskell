module Orders where

import Data.Set (Set)
import qualified Data.Set as Set 
import Data.Map (Map)
import qualified Data.Map as Map

type Customer = String
type Product = String

-- An order of some positive quantity of a product by a customer
data Order = Order Customer Product Double
    deriving (Show)

-- A delivery to the supplier of some quantity quantity of a product
data Delivery = Delivery Product Double
    deriving (Show)

-- All customers who submitted an order, with the number of different
-- products each of them ordered.
-- Strategy = Distinctive orders were required to remove duplicates so a set was used. 
    -- A list comprehension was used to remove the double component from the order. 
    -- The outcome, is a final list, where a set was required before a map for the same entries edge case 
    -- where all values for the Order are the same.
numProducts :: [Order] -> [(Customer, Int)]
numProducts orders = Map.toList $ Map.fromListWith (+) [(customer, 1) | (customer, product) <- uniqueOrders]
    where
        uniqueOrders = Set.elems $ Set.fromList [(customer, product) | (Order customer product quantity) <- orders]

-- All products that have been ordered, with the total quantity of each.
-- Strategy = A map was used as as duplicate keys can be reconsiled by 
    -- combining the values associated with the keys to ensure a single customer.
productQuantities :: [Order] -> [(Product, Double)]
productQuantities orders = Map.toList $ Map.fromListWith (+) [(product, quantity) | (Order customer product quantity) <- orders]

-- The customers and products for which the customer has ordered
-- more than half the total quantity for that product.
-- Strategy = productQuantities was required to get the total quantity, 
    -- whereby a guard was used to check for products that are greater than half the total quantity.
    -- Before equating the products from both the lists contained duplicates hence the guard.
majority :: [Order] -> [(Customer, Product)]
majority orders = [(customer, product) | (Order customer product quantity) <- orders, (product2, quantity2) <- total, quantity > (quantity2 / 2), product == product2]
    where 
        total = productQuantities orders

-- Products for which the total quantity ordered exceeds the
-- total quantity delivered, with the difference in quantity.
-- Strategy = Both the total products ordered and delivered are contained in Maps, 
    -- to allow the use of bulk map functions to acertain the difference.
    -- We used the deliveryQuantities function we initially created for the
    -- share function to avoid repition. 
    -- The guard condition ensures only products in shortfall are listed.
shortfall :: [Order] -> [Delivery] -> [(Product, Double)]
shortfall orders deliveries = [(product, double) | (product, double) <- orderProductsOnly, double > 0]
    where 
        orderTotal = Map.fromList $ productQuantities orders
        deliveryTotal = Map.fromList $ deliveryQuantities deliveries
        quantityDifference = Map.unionWith (-) orderTotal deliveryTotal
        productsNotInOrder = Map.difference deliveryTotal orderTotal
        orderProductsOnly = Map.toList $ Map.difference quantityDifference productsNotInOrder

-- Allocation of quantities of products to customers.
--
-- No customer should be allocated more of a given product than they have
-- ordered.  If a sufficient quantity of a product has been delivered to
-- satisfy all orders, each customer should receive the total quantity
-- they ordered.  If not, the available quantity of the product should
-- be shared between customers in proportion to the amount ordered.
-- For example, if the delivered quantity of a product is half the the
-- total ordered quantity of that product, each customer would receive
-- half of what they ordered.
-- Strategy = We determined that the shortfall function would be essential in answering the question, 
    -- starting from there. 
    -- We also knew that the quantity ordered and delivered would be necessary so they were converted to a Map,
    -- so bulk map functions can be used to multiply each product's quantity by a proportional scaler that determines
    -- the quantity each order can recieve based on delivery. 
    -- Seperate list comprehensions are used depending on if the order is in shortfall or not.
share :: [Order] -> [Delivery] -> [(Customer, Product, Double)]
share orders deliveries = scalingList ++ inStockList
    where
        shortfallMap = Map.fromList (shortfall orders deliveries)
        orderMap = Map.fromList (productQuantities orders)
        deliveryMap = Map.fromList $ deliveryQuantities deliveries
        scaling = Map.intersectionWith (/) (Map.intersection deliveryMap shortfallMap) orderMap
        inStock = Map.toList $ Map.difference orderMap shortfallMap
        deliveryScaling = Map.toList $ Map.difference scaling (Map.difference orderMap deliveryMap)
        scalingList = [(customer, product, quantity * scalingQuantity) | Order customer product quantity <- orders, (scalingProduct, scalingQuantity) <- deliveryScaling, product == scalingProduct]
        inStockList = [(customer, product, quantity) | Order customer product quantity <- orders, (inStockProduct, inStockquantity) <- inStock, product == inStockProduct]

-- Additional helper function for share and shortfall:
deliveryQuantities :: [Delivery] -> [(Product, Double)]
deliveryQuantities deliveries = Map.toList $ Map.fromListWith (+) [(product, quantity) | (Delivery product quantity) <- deliveries]