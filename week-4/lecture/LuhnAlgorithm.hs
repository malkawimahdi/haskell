module LuhnAlgorithm where

import Data.Char

-- valid takes a String and returns a Bool.
-- if the Int returned by checkDigit is not 0, then the card digits provided where not legitimate.
valid :: String -> Bool
valid string = checkDigit string == 0

-- checkDigit,takes a String, and returns an Int which is a modulo of 10.
-- if digit is in an odd place, then it will be summed only, whilst even numbers will have to be multiplied by 2.
    -- if the number is greater than 9, the substitution function will ensure that it is less than 9.
-- The sum of the even + odds are combines to provide a total which would be used for modulo 10.
checkDigit :: String -> Int
checkDigit string = total `mod` 10
    where
        reverse_numbers = reverse (digits string)
        total = sum (odds reverse_numbers) + sum ( [substitute numbers | numbers <- evens reverse_numbers])

-- digits takes a String and creates a list of Ints, by using list comprehension ensuring that the string is only numbers.
digits :: String -> [Int]
digits numbers = [digitToInt number | number <- numbers, isDigit number]

-- odds takes a list of anything and returns a list of anything that is odd.
-- It zips characters with their positions which are odd.
odds :: [a] -> [a]
odds anything = [char | (number, char) <- zip [1..] anything, odd number]

-- evens takes a list of anything and returns a list of anything that is even.
-- It zips characters with their positions which are even.
evens :: [a] -> [a]
evens anything = [char | (number, char) <- zip [1..] anything, even number]

-- substitute takes an Int and returns an Int.
-- It will double the input number, if it is bigger than 9, then it will subtract 9. 
substitute :: Int -> Int
substitute number
    | n2 > 9 = n2 - number
    | otherwise = n2
        where n2 = number*2