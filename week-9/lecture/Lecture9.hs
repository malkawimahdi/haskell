module Lecture9 where

-- data LTree = Leaf Int | Branch LTree LTree
--     deriving (Show)

-- sumLTree :: LTree -> Int
-- sumLTree (Leaf number) = number
-- sumLTree (Branch left right) = sumLTree left + sumLTree right

data LTree a = Leaf a | Branch (LTree a) (LTree a)
    deriving (Show)

sumLTree ::  Num a => LTree a -> a
sumLTree (Leaf number) = number
sumLTree (Branch left right) = sumLTree left + sumLTree right

doubleTree :: LTree Int -> LTree Int
doubleTree (Leaf number) = Leaf (number * 2)
doubleTree (Branch left right) = Branch (doubleTree left) (doubleTree right)

mapLTree :: (a -> b) -> LTree a -> LTree b
mapLTree f (Leaf number) = Leaf (f number)
mapLTree f (Branch left right) = Branch (mapLTree f left) (mapLTree f right)

showLTree :: LTree a -> LTree a
showLTree (Leaf number) = Leaf number
showLTree (Branch left right) = Branch (showLTree left) (showLTree right)

data NTree a = Empty | Node a (NTree a) (NTree a)
    deriving (Show)

member :: Ord a => a -> NTree a -> Bool
member something Empty = False
member something (Node current left right)
    | something < current = member something left
    | something > current = member something right
    | otherwise = True

