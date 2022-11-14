module CodeWars where

-- Complete the square sum function so that it squares each number passed into it and then sums the results together.
-- For example, for [1, 2, 2] it should return 9 because 1^2 + 2^2 + 2^2 = 9.
squareSum :: [Integer] -> Integer
squareSum ints = sum [ (int^2) | int <- ints]

-- The cockroach is one of the fastest insects.
-- Write a function which takes its speed in km per hour and returns it in cm per second, rounded down to the integer (= floored).
-- For example, 1.08 --> 30
cockroachSpeed :: Double -> Integer
cockroachSpeed km = floor $ km * 27.778

-- Write a function which calculates the average of the numbers in a given list.
-- Note: Empty arrays should return 0.
average :: [Float] -> Float
average floatList = sum [float | float <- floatList] / fromIntegral (length floatList)

-- Write function bmi that calculates body mass index (bmi = weight / height2).
-- if bmi <= 18.5 return "Underweight"
-- if bmi <= 25.0 return "Normal"
-- if bmi <= 30.0 return "Overweight"
-- if bmi > 30 return "Obese"
bmi :: Float -> Float -> String  
bmi weight height
    | bmiMeasurement <= 18.5 = "Underweight"
    | bmiMeasurement <= 25 = "Normal"
    | bmiMeasurement <= 30 = "Overweight"
    | bmiMeasurement > 30 = "Obese"
    | otherwise = "ERROR! Wrong input."
            where bmiMeasurement = weight / height ^ 2

-- We need a function that can transform a number (integer) into a string.
-- What ways of achieving this do you know?
-- Examples (input --> output):
-- 123  --> "123"
-- 999  --> "999"
-- -100 --> "-100"
numberToString :: Int -> String
numberToString number = show number

-- Consider an array/list of sheep where some sheep may be missing from their place.
-- We need a function that counts the number of sheep present in the array (true means present).
countSheep :: [Bool] -> Int
countSheep bools = length [bool | bool <- bools, True]
