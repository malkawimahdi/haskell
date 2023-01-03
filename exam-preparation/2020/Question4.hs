module Question4 where

-- d) Write a function
-- isLeaf :: Tree a -> Bool
-- that returns True if the tree is a leaf

data Tree a = Empty
| Leaf a
| Branch (Tree a) (Tree a)

isLeaf :: Tree a -> Bool
isLeaf tree
    | Leaf _ = True
    | otherwise = False