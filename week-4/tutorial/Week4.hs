{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Week4 where

import Data.Char

import Data.List

-- Expression for all the numbers from 1 to 100: [number | number <- [1..100]]
-- Expression for the squares of the numbers from 1 to 20: [number * number | number <- [1..20]]
-- Expressio for all the numbers that are divisors of 100 up to 100: [number | number <- [1..100], 100 `mod` number == 0]

-- tripleAll takes an Int List and outputs an Int List.
-- It uses list comprehensions to go through each number in the list and multiply
    -- each number by 3.
tripleAll :: [Int] -> [Int]
tripleAll numbers = [3 * number | number <- numbers]

-- squareAll takes an Int List and outputs an Int List.
-- It uses list comprehensions to go through each number in the list and multiply
    -- each number by 2.
squareAll :: [Int] -> [Int]
squareAll numbers = [2 * number | number <- numbers]

-- capitalize, takes an input String and outputs a String, whereby all characters are capitalized.  
-- It uses list comprehensions to go through each character in the list and applying the toUpper function.
capitalize :: String -> String
capitalize string = [toUpper output | output <- string]

-- capitalizeLetters, takes an input String and output a String, whereby all the characters are capitalized,
    -- if and only if the character is alpha numerical. It uses list comprehensions to go through each character
        -- in the list and applying the isAlpha function as a conditional guard.
capitalizeLetters :: String -> String
capitalizeLetters string = [toUpper output | output <- string, isAlpha output] 

-- backwards takes a String as input and outputs a String, whereby the strings are outputted in reverse order.
backwards :: String -> String
backwards string = unwords (reverse ( words string))

-- backwardsWord takes a String as input and outputs a String, whereby the order of the String,
    -- stays, but the words are reversed in the current ordering.
backwardsWords :: String -> String
backwardsWords string = unwords [ reverse one | one <- words string]

-- divisors takes a single Int and outputs a list of Ints that divide into the
    -- input Int without a remainder.
divisors :: Int -> [Int]
divisors numbers = [number | number <-[0..numbers], number `mod` 2 == 0]

-- average takes a list of Doubles and outputs a single Double representing the average of the list.  
-- average, takes the sum of the numbers in the list interpreted as Double and the length, which is converted from Int to Double,
    -- to produce an output of double.
average :: [Double] -> Double
average numbers = sum numbers / fromIntegral (length numbers)

-- isPalindrome takes a String as input and returns a Bool based on if the word is a palindrome.  
isPalindrome :: String -> Bool
isPalindrome string = reverse string == string

-- isFamousPalindrome takes a String as input and returns a Bool based on if the word is a palindrome.
-- isPalindrome sanitizes the input for anything that isn't a character and calls isPalindrome on the output.
isFamousPalindrome :: String -> Bool
isFamousPalindrome string = isPalindrome (capitalizeLetters string)

-- frequency takes an ordered list of anything (that is the same type) and returns the occurances of each thing in the list.
frequency :: Ord a => [a] -> [(a, Int)]
frequency chars = [(head grouping, length grouping) | grouping <- group(sort chars)]

-- palindromic :: [Char] -> Bool
-- palindromic string
--     | length temp > 1 = False
--     | otherwise = True
--         where temp = [char | (char, number) <- frequency string, odd number]

-- palindromic takes a list of char and outputs a bool representing if the string was a palindrome,
    -- in any possible combination. If there are 0 or 1 odd characters, then it is True, otherwise False.
palindromic :: [Char] -> Bool
palindromic string =  length ([char | (char, number) <- frequency string, odd number]) <= 1

