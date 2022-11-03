module Week5 where

import Data.Char

-- capitalize, takes an input String and outputs a String, whereby all characters are capitalized.  
-- It uses list comprehensions to go through each character in the list and applying the toUpper function.
capitalize :: String -> String
capitalize = map toUpper

-- capitals takes an input String and outputs the letters that are capitalized within the String.
-- capitals uses a Higher - order function to achieve this functionality.
capitals :: String -> String
capitals = filter isUpper

-- capitalizeLetters, takes a String as input and outputs a String
-- capitalizeLetters, uses the infix operator ($), to first apply the Higher - order function filter,
    -- then to the string checking for alpha numerical characters. The output of that, is the input
    -- for the second parameter of map. 
capitalizeLetters :: String -> String
capitalizeLetters string =  map toUpper $ filter isAlpha string

-- squaresTwenty, takes no arguments, and outputs a list of Int's containing the squares of the first 20 numbers.
-- squaresTwenty, uses an anonymous function to achieve this functionality in conjunction with the map function.
squaresTwenty :: [Int]
squaresTwenty = map (\number -> number * number) [1..20]

-- squaresLessThanFiveHundred, takes no arguments, and outputs a list of Int's containing the squares of the numbers below 500.
-- squaresLessThanFiveHundred, uses an anonymous function to achieve this functionality in conjunction with the map & takeWhile functions.
squaresLessThanFiveHundred :: [Int]
squaresLessThanFiveHundred = map (\number -> number * number) $ takeWhile (\number -> number * number < 500) [1..]

-- squaresBetweenFiveHundredAndOneThousand, takes no arguments, and outputs a list of Int's containing the squares of the numbers between 500 and 1000.
-- squaresBetweenFiveHundredAndOneThousand, uses an anonymous function to achieve this functionality in conjunction with the map & takeWhile functions.
squaresBetweenFiveHundredAndOneThousand :: [Int]
squaresBetweenFiveHundredAndOneThousand = map (\number -> number * number) $ takeWhile (\number -> number * number > 500 && number * number < 1000) [23..]

-- count takes anything with the constraint that you can apply equality to. 
-- count takes two parameters, the search value a, and a list of values the same type of a.  
-- count, will count the occurence of the search value within the list.
count :: Eq  a => a -> [a] -> Int
count search_number list = length $ filter (\number -> number == search_number) list

-- f takes a list of Integers as input and outputs another list of Integers.  
-- f, uses function composition to raise any number within the list to ^2, then applies filter to any value less than 20.
f :: [Integer] -> [Integer]
f = filter (( <20) . (^2))

-- ff takes a list of Floats and outputs a list of Floats.  
-- for each float, one is added to the number and then it is divided by 2.
-- ff :: [Float] -> [Float]
-- ff xs = map (\x -> (x+1) / 2) xs

-- ff takes a list of Floats and outputs a list of Floats.  
-- for each float, one is added to the number and then it is divided by 2.
-- This implementation differs from the ff commented, as it uses function composition. 
ff :: [Float] -> [Float]
ff = map ((/2) . (+1))

-- foo takes a list of Ints, and returns a list of Ints, whereby the Higher - order function zipWith,
    -- uses the binary operator (-) to subtract the second number onwards from the list, from the orginal list.
-- foo subtracts the current element, from the previous element of the same list, subtracting the difference.
foo :: [Int] -> [Int]
foo xs = zipWith (-) (tail xs) xs

-- collatz takes an Int as input and outputs an Int.
-- If the Int is even, then it is divided by 2 and outputted.
-- If the Int is odd, then it is multiplies by 3 and 1 is added.
collatz :: Int -> Int
collatz number
    | even number = number `div` 2
    | otherwise = 3 * number + 1

-- collatzSteps, takes an Int and outputs the number of times that collatz has to be applied to reach 1.
collatzSteps :: Int -> Int
collatzSteps number = length $ takeWhile (\number -> number /= 1) $ iterate collatz number

-- collatzMax, applies collatz to the input number, and outputs the largest number applying the function until 1 is reached.
collatzMax :: Int -> Int
collatzMax number = maximum $ takeWhile (\number -> number /= 1) $ iterate collatz number

-- pascal, takes no input and outputs a list of list containing Integers.
pascal :: [[Integer]]
pascal = iterate pascalOnce [1]

-- pascalOnce, takes a list of Integers, and outputs a list of Integers.
-- pascalOnce, calculates the next row for the pascal triangle from the list of Integers. 
pascalOnce :: [Integer] -> [Integer]
pascalOnce nums = zipWith (+) ([0] ++ nums) (nums ++ [0])

-- showPascal, takes no input and calls pascal 10 times and outputs the pascal triangle in console.
showPascal :: IO ()
showPascal = putStr $ unlines $ map unwords $ map (map show) $ take 10 pascal