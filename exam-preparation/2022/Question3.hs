module Question3 where

data Tree a = Node a [Tree a]
    deriving (Show)

-- b) Write a function that returns the root of the tree.
root :: Tree a -> a
root (Node node [subtree]) = node

-- c) Write a function that returns the number of children of the root tree.
arity :: Tree a -> Int
arity(Node node [subtree]) = length subtree

-- d) Write a function that returns the sum of the integers in a tree.
sumTree :: Tree Int -> Int
sumTree (Node node [subtree]) = node + sumTree subtree