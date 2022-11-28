module Week8 where

import Data.Char

-- foo creates an inital frame that calls the recursive function, bar, which removes sequential duplicates.
foo :: Eq a => [a] -> [a]
foo [] = []
foo (x:xs) = x : bar x xs 

bar :: Eq a => a -> [a] -> [a]
bar x [] = []
bar x (y:ys)
    | x == y = bar x ys
    | otherwise = y : bar y ys


-- removeFirstDigit takes a list of chars and ouputs a list of chars, removing the first character within the list of chars, ouputting
    -- a new list without the digit.
removeFirstDigit :: [Char] -> [Char]
removeFirstDigit (x:xs)
    | isDigit x = xs
    | otherwise = x : removeFirstDigit xs
removeFirstDigit [] = []

-- removeFirst takes a function and a list to output a list that removes the first elemnt of the list that does
    -- not satisfy the first property of the function, keeping the remainder of the list.
removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst p [] = []
removeFirst p (x:xs)
  | p x          = xs
  | otherwise    = x : removeFirst p xs

-- addLists takes a two lists of numbers and combines the values where possible. 
    -- If a list exceeds the length of the other list, then the values willl be placed behind.
addLists :: Num a => [a] -> [a] -> [a]
addLists [] [] = []
addLists (x:xs) [] = x : addLists [] xs
addLists [] (y:ys) = y : addLists ys []
addLists  (x:xs) (y:ys) = (x + y) : addLists xs ys

-- longZip takes a function and two lists to output a list which has operated on both lists using function f.
longZip :: (a -> a -> a) -> [a] -> [a] -> [a]
longZip f [] [] = []
longZip f xs [] = xs 
longZip f [] ys = ys
longZip f (x:xs) (y:ys) = f x y : longZip f xs ys

-- merge takes two ordered lists of anything and generates a list that has ordered the input lists into a single output list. 
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x < y = x : merge xs (y : ys)
    | otherwise = y : merge (x : xs) ys

-- odds takes anything and outputs the values in odd number positioning.
odds :: [a] -> [a]
odds [] = []
odds [x] = [x]
odds (x1:x2:xs) = x1:odds xs

-- evens takes anything and outputs the values in even number positioning.
evens :: [a] -> [a]
evens [] = []
evens [x] = []
evens (x1:x2:xs) = x2:evens xs