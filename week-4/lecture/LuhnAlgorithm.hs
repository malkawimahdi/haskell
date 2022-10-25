module LuhnAlgorithm where

import Data.Char

valid :: String -> Bool
valid string = checkDigit string == 0

checkDigit :: String -> Int
checkDigit string = total `mod` 10
    where
        reverse_numbers = reverse (digits string)
        total = sum (odds reverse_numbers) + sum ( [substitute numbers | numbers <- evens reverse_numbers])

digits :: String -> [Int]
digits numbers = [digitToInt number | number <- numbers, isDigit number]

odds :: [a] -> [a]
odds xs = [c | (n, c) <- zip [1..] xs, odd n]

evens :: [a] -> [a]
evens xs = [c | (n, c) <- zip [1..] xs, even n]

substitute :: Int -> Int
substitute number
    | n2 > 9 = n2 - number
    | otherwise = n2
        where n2 = number*2