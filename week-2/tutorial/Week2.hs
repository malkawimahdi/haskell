module Week2 where

import Data.Char

-- threeDifferent takes 3 Integers as paramaters, and checks if all 3 Integers are different from each other.
threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent number1 number2 number3 
    | number1 == number2  = False
    | number2 == number3  = False
    | number1 == number3  = False
    | otherwise           = True

-- mystery function takes 3 Int's as parameters and returns a bool.
-- If the numbers inputted to the function are equal to one another.
-- Then the function will take the True input and reverse it to become False using not.
-- The difference between this function and mystery is, if the numbers are equal within this function,
    -- it will output False, whilst if the inputs are not equal to one another, then the output is True. 
-- This function does the inverse of this function.  
mystery :: Int -> Int -> Int -> Bool
mystery number1 number2 number3 = not (number1 == number2 && number2 == number3)

-- newMistery functions produces the same output as the mystery function.
-- However, the implementation of each function is different.
-- newMistery implements the same functionality, without using not.
newMistery :: Int -> Int -> Int -> Bool
newMistery number1 number2 number3 
    | number1 == number2 && number2 == number3 = False
    | otherwise                                = True 

-- fractionalComputation takes a RealFrac and outputs a RealFrac. 
-- fractionalComputation computes the fractional part of a RealFrac and outputs it to ghci.   
fractionalComputation :: RealFrac a => a -> a
fractionalComputation number = number - fromIntegral(floor number)

-- Write a function clamp that takes three arguments, low, high and x (all of type Double). 
-- It returns x if it is between low and high (inclusive), low if x is less than low,
    -- and high if x is greater than high. 
-- You may assume low <= high.   
-- clamp takes three arguements, all of which are of type Double and it returns Double.
clamp :: Double -> Double -> Double -> Double
clamp low high x
    | low <= x && x <= high = x 
    | x <= low              = low 
    | x >= high             = high

-- Write a function with the function signature charToNum :: Char -> Int.  
-- It should convert a character that represents a digit, like '3, to the corresponding integer (3),
    -- or 0 if the character does not represent a digit. You may assume that the digit characters are contiguous
        -- and in ascending order.  
-- charToNum takes as input a character, and outputs it's integer value as result.   
charToNum :: Char -> Int
charToNum character
    | (ord character - ord '0') < 10 = ord character - ord '0'
    | otherwise                      = 0