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

-- continue from removeFirst
removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst f [] = []
removeFirst f (x:[]) = f x
removeFirst f (x:xs) = x : removeFirst f xs
