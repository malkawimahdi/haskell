module Week9 where

-- data definition, where an LTree, can either be a Leaf or a Branch that connects to other LTree's.
data LTree a = Leaf a | Branch (LTree a) (LTree a)
    deriving (Show)

-- Test cases for functions, specifically LTree's of type Int.
tree1, tree2, tree3 :: LTree Int
tree1 = Leaf 3
tree2 = Branch (Leaf 3) (Leaf 7)
tree3 = Branch (Leaf 2) (Branch (Leaf 3) (Leaf 7))

-- lTreeNumberOfLeaves, takes an LTree as input and recursively sums the number of leaf nodes.
lTreeNumberOfLeafs :: LTree a -> Int
lTreeNumberOfLeafs (Leaf something) = 1
lTreeNumberOfLeafs (Branch left right) = lTreeNumberOfLeafs left + lTreeNumberOfLeafs right

-- lTreeDepth, takes an LTree as input and recursively calculates the length of the branches, outputting the largest branch.
lTreeDepth :: LTree a -> Int
lTreeDepth (Leaf something) = 1
lTreeDepth (Branch left right) = 1 + max (lTreeDepth left) (lTreeDepth right)

-- lTreeTotalLeafValue, has a constraint in order to operate, that the LTree used, is of a Number type,
    -- whereby it recursively calls lower levels until the base case is reached summing the value.
lTreeTotalLeafValue :: Num a => LTree a -> a
lTreeTotalLeafValue (Leaf number) = number
lTreeTotalLeafValue (Branch left right) = lTreeTotalLeafValue left + lTreeTotalLeafValue right

--lTreeLeafValues, takes as input an lTree and outputs a list containing all the values found at the leaf nodes,
    -- in order to how they have been located.
lTreeLeafValues :: LTree a -> [a]
lTreeLeafValues (Leaf number) = [number]
lTreeLeafValues (Branch left right) = lTreeLeafValues left ++ lTreeLeafValues right

-- lTreeMirror, takes any LTree as input and mirrors the contents as output.
lTreeMirror :: LTree a -> LTree a
lTreeMirror (Leaf something) = Leaf something
lTreeMirror (Branch left right) = Branch (lTreeMirror right) (lTreeMirror left)

-- sumLTree acts exactly as lTreeTotalLeafValue
sumLTree ::  Num a => LTree a -> a
sumLTree (Leaf number) = number
sumLTree (Branch left right) = sumLTree left + sumLTree right

-- foldLTree, takes a function that outputs the same type and any LTree, to fold the LTree, into a single value,
    -- using the function that has been supplied.
foldLTree :: (a -> a -> a) -> LTree a -> a
foldLTree f (Leaf something) = something
foldLTree f (Branch left right) =  f (foldLTree f left) (foldLTree f right)

-- redefinedSumLTree has a constraint that the input LTree, is of a Number type and performs foldLTree on the input,
    -- to produce the sum of all the leafs.
redefinedSumLTree ::  Num a => LTree a -> a
redefinedSumLTree lTree = foldLTree (+) lTree

-- mapLTree, takes a function from Type a to Type b and an LTree of Type a, to output an LTree of Type b,
    -- applying the function to each node.
mapLTree :: (a -> b) -> LTree a -> LTree b
mapLTree f (Leaf number) = Leaf (f number)
mapLTree f (Branch left right) = Branch (mapLTree f left) (mapLTree f right)

redefinedLTreeNumberOfLeafs :: LTree a -> Int
redefinedLTreeNumberOfLeafs lTree = sumLTree (mapLTree f lTree)
    where
        f = const 1
-- redefinedLTreeNumberOfLeafs f lTree = f (const sumLTree . mapLTree (+) lTree)