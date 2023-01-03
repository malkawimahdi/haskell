module Question2 where

import Data.Char
import System.Directory
import Data.List

-- a) Define a function theat returns True for lists of 2 or more elements, and False otherwise
multiple :: [a] -> Bool
list
    | (length list) >=2 = True
    | otherwise = False

-- b) Consider the following function definition
-- mystery :: Ord a => a -> [a] -> [a]
-- mystery x [] = [] (Defines base case 1)
-- mystery x (y:ys)
-- | x < y = y : ys
-- | otherwise = mystery x ys
-- Define an equivalent function, but without using recursion.

mystery :: Ord a => a -> [a] -> [a]
mystery x [] = []
mystery x (y:ys) = dropWhile (x < y) ys
 

-- c) Give list comprehensions (without higher-order functions) that are equivalent to the following expressions:
-- i) map (+1) (filter (<= 50) xs) 
-- Answer: [ x+1 | x <- xs, x <= 50]
 
-- ii) map (3*) (filter ((< 100) . (^2)) xs) 
-- Answer: [x*3 | x<-xs, x*2 ,x < 100]

-- d) Define a function that adds adjacent elements of a list.
-- addPairs [] = []
-- addPairs [x] = [x]
-- addPairs (x1:x2:xs) = x1 + x2 : addPairs xs

-- e) Define a generalised function of the same type.
-- mapPairs f [] = []
-- mapPairs f [x] = [x]
-- mapPairs f (x1:x2:xs) = f x1 x2 : mapPairs f xs