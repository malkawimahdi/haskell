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
numProducts :: [Order] -> [(Customer, Int)]
numProducts orders = Map.toList $ Map.fromListWith (+) [(customer, 1) | (customer, product) <- uniqueOrders]
    where
        uniqueOrders = Set.elems $ Set.fromList [(customer, product) | (Order customer product double) <- orders]
-- numProducts orders = Map.toList $ Map.fromListWith (+) [(c, 1) | (Order c p d) <- orders]

-- All products that have been ordered, with the total quantity of each.
productQuantities :: [Order] -> [(Product, Double)]
productQuantities orders = Map.toList $ Map.fromListWith (+) [(p, d) | (Order c p d) <- orders]


tmp :: [Order] -> [(Customer, Double)]
tmp orders = Map.toList $ Map.fromListWith (+) [(c, d) | (Order c p d) <- orders]

-- The customers and products for which the customer has ordered
-- more than half the total quantity for that product.
-- majority :: [Order] -> [(Customer, Product)]
-- majority orders = Map.toList $ Map.fromListWith (+) [(c, p) | (Order c p d) <- orders]



-- majority :: [Order] -> [(Customer, Product)]
-- majority orders = filter ([double > d / 2 | (Order customer product double) <- orders, (p,d) <- total]) orders
--     where
--         total = productQuantities orders
--         -- prodCount = [ d > (t /2) | d <- Order c p d, t <- total]


-- -- [double > d / 2 | (Order customer product double) <- order, (p,d) <- total]
-- isTrue :: Order -> [Order] -> Bool
-- isTrue singleOrder orders = [ | (Order customer product double) <- order, (p,d) <- total]
--     where
--         total = productQuantities orders


majority :: [Order] -> [(Customer, Product)]
majority orders = [(customer, product) | (Order customer product double) <- orders, (product2, double2) <- total, double > double2 / 2, product2 == product]
    where 
        total = productQuantities orders
        -- prodCount = [ d > (t /2) | d <- Order c p d, t <- total]


-- -- The customers and products for which the customer has ordered
-- -- more than half the total quantity for that product.
-- majority :: [Order] -> [(Customer, Product)]
-- majority orders = Set.elems Set.filter (a -> Bool) $ Set.fromList $ [(customer, product) | (Order customer product double) <- orders, (p, d) <- total, double > d / 2]
--     where 
--         total = productQuantities orders
--         prodCount = [ d > (d /2) | Order customer product double <- orders, (p, d) <- total]

deliveryQuantities :: [Delivery] -> [(Product, Double)]
deliveryQuantities deliveries = Map.toList $ Map.fromListWith (+) [(p, d) | (Delivery p d) <- deliveries]

-- Products for which the total quantity ordered exceeds the
-- total quantity delivered, with the difference in quantity.
-- shortfall1 :: [Order] -> [Delivery] -> [(Product, Double)]
-- shortfall1 orders deliveries = [(product, double) | (product, double) <- orderProductsOnly]
--     where 
--         orderTotal = Map.fromListWith (+) [(product, double) | (Order customer product double) <- orders]
--         deliveryTotal = Map.fromListWith (+) [(product, double) | (Delivery product double) <- deliveries]
--         quantityDifference = Map.unionWith (-) orderTotal deliveryTotal
--         productsNotInOrder = Map.difference deliveryTotal orderTotal
--         orderProductsOnly = Map.toList $ Map.difference quantityDifference productsNotInOrder

isPositive :: (Num a, Ord a) => a -> Bool
isPositive num
    | num > 0 = True
    | otherwise = False
    
shortfall :: [Order] -> [Delivery] -> [(Product, Double)]
shortfall orders deliveries = [(product, double) | (product, double) <- orderProductsOnly, double > 0]
    where 
        orderTotal = Map.fromListWith (+) [(product, double) | (Order customer product double) <- orders]
        deliveryTotal = Map.fromListWith (+) [(product, double) | (Delivery product double) <- deliveries]
        quantityDifference = Map.unionWith (-) orderTotal deliveryTotal
        productsNotInOrder = Map.difference deliveryTotal orderTotal
        orderProductsOnly = Map.toList $ Map.difference quantityDifference productsNotInOrder



-- shortfall :: [Order] -> [Delivery] -> [(Product, Double)]
-- shortfall orders deliveries = [(product, double) | (product, double) <- orderProductsOnly]
--      where 
--          orderTotal = Map.fromListWith (+) [(product, double) | (Order customer product double) <- orders]
--          deliveryTotal = Map.fromListWith (+) [(product, double) | (Delivery product double) <- deliveries]
--          quantityDifference = Map.unionWith (-) deliveryTotal orderTotal
--          productsNotInOrder = Map.difference deliveryTotal orderTotal
--          orderProductsOnly = Map.toList $ Map.map abs $ Map.difference quantityDifference productsNotInOrder

    
-- isPositive :: (Num a, Ord a) => a -> Bool
-- isPositive num
--     | num >= 0 = True
--     | otherwise = False

-- shortfall :: [Order] -> [Delivery] -> [(Product, Double)]
-- shortfall orders deliveries = [(product, double) | (Order customer product double) <- orders, (Delivery product2 double2) <- deliveries]
--     where 
--         orderTotal = Map.fromListWith (+) [(p, d) | (Order c p d) <- orders]
--         deliveryTotal = Map.fromListWith (+) [(p, d) | (Delivery p d) <- deliveries]
-- Allocation of quantities of products to customers.


-- shortfall :: [Order] -> [Delivery] -> [(Product, Double)]
-- shortfall orders deliveries = [(product, double) | (product, double) <- quantityDifference]
--     where 
--         orderTotal = Map.fromListWith (+) [(p, d) | (Order c p d) <- orders]
--         deliveryTotal = Map.fromListWith (+) [(p, d) | (Delivery p d) <- deliveries]
--         quantityDifference = Map.toList $ Map.unionWith (-) orderTotal deliveryTotal
--         productsNotInOrder = Map.difference deliveryTotal orderTotal
--         orderProductsOnly = Map.difference quantityDifference productsNotInOrder


--
-- No customer should be allocated more of a given product than they have
-- ordered.  If a sufficient quantity of a product has been delivered to
-- satisfy all orders, each customer should receive the total quantity
-- they ordered.  If not, the available quantity of the product should
-- be shared between customers in proportion to the amount ordered.
-- For example, if the delivered quantity of a product is half the the
-- total ordered quantity of that product, each customer would receive
-- half of what they ordered.
share :: [Order] -> [Delivery] -> [(Customer,Product, Double)]
share orders deliveries = list1 ++ list2
    where
        shortFallOrders = shortfall orders deliveries 
        shortFallOrdersMap = Map.fromList  shortFallOrders
        shortFallOrdersSet = Set.fromList  [p | (p, d) <- shortFallOrders]
        deliveryQty = Map.intersection (Map.fromList $ deliveryQuantities deliveries) shortFallOrdersMap
        deliveryThatMeetsShortfall = Map.union deliveryQty shortFallOrdersMap
        productQty = Map.fromList $ productQuantities orders
        uniqueProducts = Map.intersection productQty shortFallOrdersMap
        scalingMap =  Map.assocs $ Map.unionWith (/) deliveryThatMeetsShortfall uniqueProducts
        scalingMap2 = Map.unionWith (/) deliveryThatMeetsShortfall uniqueProducts
        orderProductSet = Set.fromList $ [product | (product, double ) <- productQuantities orders]
        deliveryProductSet = Set.fromList $ [product | (product, double ) <- deliveryQuantities deliveries]
        productNotInDeliverySet = Set.difference orderProductSet deliveryProductSet
        list1 = [(customer, product, double) | Order customer product double <- orders, not $ Set.member product shortFallOrdersSet, not $ Set.member product productNotInDeliverySet]
        list2 = [(customer, product, double * d) | Order customer product double <- orders, (p, d) <- scalingMap, Map.member product scalingMap2, p == product, not $ Set.member product productNotInDeliverySet]

        -- look at shortFall, as it currently allocates orders for items that have recieved no delivery.
        -- code below fixes edge-case where delivery does not include product, therefore it cannot be included in output.
        
        





-- share2 :: [Order] -> [Delivery] -> [(Customer,Product, Double)]
-- share2 orders deliveries =  list1 ++ list2
--     where
--         shortFallOrders = shortfall orders deliveries
--         shortFallOrdersMap = Map.fromList  shortFallOrders
--         shortFallOrdersSet = Set.fromList  [p | (p, d) <- shortFallOrders]
--         deliveryQty = Map.intersection (Map.fromList $ deliveryQuantities deliveries) shortFallOrdersMap
--         deliveryThatMeetsShortfall = Map.union deliveryQty shortFallOrdersMap
--         productQty2 = Map.fromList $ productQuantities orders
--         uniquePRODUCTS = Map.intersection productQty2 shortFallOrdersMap
--         scalingMap =  Map.assocs $ Map.unionWith (/) deliveryThatMeetsShortfall uniquePRODUCTS
--         scalingMap2 = Map.unionWith (/) deliveryThatMeetsShortfall uniquePRODUCTS
--         list1 = [(customer, product, double) | Order customer product double <- orders, not $ Set.member product shortFallOrdersSet, not $ Set.member product productNotInDeliverySet]
--         list2 = [(customer, product, double * d) | Order customer product double <- orders, (p, d) <- scalingMap, Map.member product scalingMap2, p == product]


        -- shortFallOrdersSet = Set.fromList $ shortFallOrders
        -- productsWithNotEnoughQty = Set.fromList $ [p | (p, d) <- shortFallOrders] -- All products in shortfall
        -- deliveryProducts = Set.fromList [product | (Delivery product double) <- deliveries] --all products from delivery
        -- orderProductNotInDelivery = Set.difference productsWithNotEnoughQty deliveryProducts -- all products with not enough qty and not in delivery.
        -- deliveryProductsThatContainShortfalls = Set.intersection productsWithNotEnoughQty deliveryProducts
        -- deliveryProductsThatContainShortfallsSet = [(product, double) | (Delivery product double) <- deliveries, Set.member product deliveryProductsThatContainShortfalls]
        -- --scaling = [(p, (d / d2) * d)| (p, d)<- deliveryProductsThatContainShortfallsMap,  (p2, d2) <- shortFallOrders , p == p2]
        -- scaling = [(p, (d / d2) * d)| (p, d)<- deliveryProductsThatContainShortfallsSet,  (p2, d2) <- shortFallOrders , p == p2, (Order customer product double) <- orders, not $ Set.member product productsWithNotEnoughQty, not $ Set.member product orderProductNotInDelivery]


-- share :: [Order] -> [Delivery] -> [(Customer, Product, Double)]
-- share orders deliveries = [(customer, product, double) | (Order customer product double) <- orders, not $ Set.member product productsWithNotEnoughQty]
--     where
--         shortFallOrders = shortfall orders deliveries
--         productsWithNotEnoughQty = Set.fromList $ [p | (p, d) <- shortFallOrders]

        

-- share orders deliveries = [(customer, product, double) | (Order customer product double) <- orders, Set.member product orderWithEnoughQty]
--     where
--         orderTotal = Map.fromListWith (+) [(product, double) | (Order customer product double) <- orders]
--         deliveryTotal = Map.fromListWith (+) [(product, double) | (Delivery product double) <- deliveries]
--         shortFallOrders = shortfall orders deliveries
--         shortFallProducts = Set.fromList $ shortfall orders deliveries 
--         orderWithEnoughQty = Set.fromList $ [  p | (Order c p d) <- orders, not $ Set.member p allOrderedProducts]
--         allOrderedProducts = Set.fromList $ [p | (Order c p d) <- orders]
--         ordersThatCanBeDone = Set.difference allOrderedProducts orderWithEnoughQty

-- share :: [Order] -> [Delivery] -> [(Customer, Product, Double)]
-- share orders deliveries = [(customer, product, double) | (Order customer product double) <- orders]
--     where
--         orderTotal = Map.fromListWith (+) [(product, double) | (Order customer product double) <- orders]
--         deliveryTotal = Map.fromListWith (+) [(product, double) | (Delivery product double) <- deliveries]
--         shortFallOrders = shortfall orders deliveries
--         orderWithEnoughQty = [  p | (Order c p d) <- orders, (p2, d) <- shortFallOrders, p /= p2]