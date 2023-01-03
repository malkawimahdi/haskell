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
-- This function will do the same without using sets? The map already takes the unique value for the customers

-- Strategy = Distinctive orders were required to remove duplicates so a set was used. 
    -- A list comprehension was used to remove the double component from the order. 
    -- The outcome, is a final list, where a set was required before a map for the same entries edge case 
    -- where all values for the Order are the same.
numProducts :: [Order] -> [(Customer, Int)]
numProducts orders = Map.toList $ Map.fromListWith (+) [(customer, 1) | (customer, product) <- uniqueOrders]
    where
        uniqueOrders = Set.elems $ Set.fromList [(customer, product) | (Order customer product double) <- orders]

-- All products that have been ordered, with the total quantity of each.

-- Strategy = A map was used as as duplicate keys can be reconsiled by 
    -- combining the values associated with the keys to ensure a single customer. 
productQuantities :: [Order] -> [(Product, Double)]
productQuantities orders = Map.toList $ Map.fromListWith (+) [(product, double) | (Order customer product double) <- orders]

-- The customers and products for which the customer has ordered
-- more than half the total quantity for that product.

-- Strategy = productQuantities was required to get the total quantity, 
    -- whereby a guard was used to check for products that are greater than half the total quantity.
    -- Before equating the products from both the lists contained duplicates hence the guard.
majority :: [Order] -> [(Customer, Product)]
majority orders = [(customer, product) | (Order customer product double) <- orders, (product2, double2) <- total, double > double2 / 2, product2 == product]
    where 
        total = productQuantities orders

-- Products for which the total quantity ordered exceeds the
-- total quantity delivered, with the difference in quantity.

-- Strategy = Both the total products ordered and delivered are contained in Maps, 
    -- to allow the use of bulk map functions to acertain the difference. 
    -- The guard condition ensures only products in shortfall are listed. 
shortfall :: [Order] -> [Delivery] -> [(Product, Double)]
shortfall orders deliveries = [(product, double) | (product, double) <- orderProductsOnly, double > 0]
    where 
        orderTotal = Map.fromListWith (+) [(product, double) | (Order customer product double) <- orders]
        deliveryTotal = Map.fromListWith (+) [(product, double) | (Delivery product double) <- deliveries]
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
share orders deliveries = list1 ++ list2
    where
        shortFallOrders = shortfall orders deliveries 
        shortFallOrdersMap = Map.fromList  shortFallOrders
        shortFallOrdersSet = Set.fromList  [p | (p, d) <- shortFallOrders]
        deliveryQty = Map.intersection (Map.fromList $ deliveryQuantities deliveries) shortFallOrdersMap
        deliveryThatMeetsShortfall = Map.union deliveryQty shortFallOrdersMap
        productQty = Map.fromList $ productQuantities orders
        uniqueProducts = Map.intersection productQty shortFallOrdersMap
        shortFallProductQty =  Map.fromList shortFallOrders
        totalProductQty = Map.intersectionWith (+) productQty shortFallProductQty
        scalingMap =  Map.assocs $ Map.unionWith (/) deliveryThatMeetsShortfall uniqueProducts
        scalingMap2 = Map.unionWith (/) deliveryThatMeetsShortfall uniqueProducts
        orderProductSet = Set.fromList $ [product | (product, double ) <- productQuantities orders]
        deliveryProductSet = Set.fromList $ [product | (product, double ) <- deliveryQuantities deliveries]
        productNotInDeliverySet = Set.difference orderProductSet deliveryProductSet
        list1 = [(customer, product, double) | Order customer product double <- orders, not $ Set.member product shortFallOrdersSet, not $ Set.member product productNotInDeliverySet]
        list2 = [(customer, product, double * d) | Order customer product double <- orders, (p, d) <- scalingMap, Map.member product scalingMap2, p == product, not $ Set.member product productNotInDeliverySet]


-- Additional helper function for the share function:
deliveryQuantities :: [Delivery] -> [(Product, Double)]
deliveryQuantities deliveries = Map.toList $ Map.fromListWith (+) [(p, d) | (Delivery p d) <- deliveries]