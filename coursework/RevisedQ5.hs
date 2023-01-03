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

-- Strategy = The unique orders were required as we didn't want duplicates so we used a set and removed the double. 
--            That allowed us to get the final list. A set was required before a map for the duplicate edge case where all values were the same.
numProducts :: [Order] -> [(Customer, Int)]
numProducts orders = Map.toList $ Map.fromListWith (+) [(customer, 1) | (customer, product) <- uniqueOrders]
    where
        uniqueOrders = Set.elems $ Set.fromList [(customer, product) | (Order customer product double) <- orders]

-- All products that have been ordered, with the total quantity of each.

-- Strategy = We used a map since it had unique keys. Therefore we could avoid duplicate values and omit the customer from the output.
productQuantities :: [Order] -> [(Product, Double)]
productQuantities orders = Map.toList $ Map.fromListWith (+) [(product, double) | (Order customer product double) <- orders]

-- The customers and products for which the customer has ordered
-- more than half the total quantity for that product.

-- Strategy = We determined that productQuantities was needed to get the total quantity,
--            then we put in the guard for over half the total quantity. Before equating the products 
--            from both the lists we noticed duplicates so that's why we added that guard.
majority :: [Order] -> [(Customer, Product)]
majority orders = [(customer, product) | (Order customer product double) <- orders, (product2, double2) <- total, double > double2 / 2, product2 == product]
    where 
        total = productQuantities orders

-- Products for which the total quantity ordered exceeds the
-- total quantity delivered, with the difference in quantity.

-- Strategy = We needed to get both the total products ordered and the products delivered in maps.
--            This allowed us to use Map functions to get the actual difference. We added the 
--            (double > 0) guard so we'd only get the products in shortfall.
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

-- Strategy = We determined that the shortfall function would be essential for this so we
--            started from there. We also knew that the quantity ordered and delivered would be 
--            necessary so we applied Map functions. To multiply by a proportional amount,
--            we added scaling variables and combined the lists together. We created a helper function
--            below (similar to productQuantities) to help with comparisons.
share :: [Order] -> [Delivery] -> [(Customer, Product, Double)]
share orders deliveries = [(customer, product, quantity) | (customer, (product, quantity)) <- Map.assocs combinationMap]
    where
        shortfallMap = Map.fromList (shortfall orders deliveries)
        orderMap = Map.fromList (productQuantities orders)
        deliveryMap = Map.fromList $ deliveryQuantities deliveries
        scaling = Map.intersectionWith (/) (Map.intersection deliveryMap shortfallMap) orderMap
        inStock = Map.difference orderMap shortfallMap
        deliveryScaling = Map.toList $ Map.difference scaling (Map.difference orderMap deliveryMap)
        scalingMap = Map.fromList [(customer, (product, quantity * scalingQuantity)) | Order customer product quantity <- orders, (scalingProduct, scalingQuantity) <- deliveryScaling, product == scalingProduct]
        inStockMap = Map.fromList [(customer, (product, quantity)) | Order customer product quantity <- orders, Map.member product inStock]
        combinationMap = Map.union scalingMap inStockMap
        -- scalingMap = Map.fromList [(customer, (product, quantity)) | Order customer product quantity <- orders, ]\\

-- share orders deliveries = list1 ++ list2
--     where
--         shortFallOrders = shortfall orders deliveries 
--         shortFallOrdersMap = Map.fromList  shortFallOrders
--         shortFallOrdersSet = Set.fromList  [p | (p, d) <- shortFallOrders]
--         deliveryQty = Map.intersection (Map.fromList $ deliveryQuantities deliveries) shortFallOrdersMap
--         deliveryThatMeetsShortfall = Map.union deliveryQty shortFallOrdersMap
--         productQty = Map.fromList $ productQuantities orders
--         uniqueProducts = Map.intersection productQty shortFallOrdersMap
--         scalingMap =  Map.assocs $ Map.unionWith (/) deliveryThatMeetsShortfall uniqueProducts
--         scalingMap2 = Map.unionWith (/) deliveryThatMeetsShortfall uniqueProducts
--         orderProductSet = Set.fromList $ [product | (product, double ) <- productQuantities orders]
--         deliveryProductSet = Set.fromList $ [product | (product, double ) <- deliveryQuantities deliveries]
--         productNotInDeliverySet = Set.difference orderProductSet deliveryProductSet
--         list1 = [(customer, product, double) | Order customer product double <- orders, not $ Set.member product shortFallOrdersSet, not $ Set.member product productNotInDeliverySet]
--         list2 = [(customer, product, double * d) | Order customer product double <- orders, (p, d) <- scalingMap, Map.member product scalingMap2, p == product, not $ Set.member product productNotInDeliverySet]


-- Additional helper function for the share function:
deliveryQuantities :: [Delivery] -> [(Product, Double)]
deliveryQuantities deliveries = Map.toList $ Map.fromListWith (+) [(p, d) | (Delivery p d) <- deliveries]