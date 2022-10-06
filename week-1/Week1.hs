module Week1 where

--Constant function that has no input, but only outputs
--Single Integer means that that function has NO INPUTS and only outputs
size :: Integer
size = 12 + 13

--The square of an Integer.
square :: Num a => a -> a
square number = number * number

--Triples the value of the input Integer.
-- => Seperates a constraint from a type.
triple :: Num a => a -> a
triple number = number * 3

--Square of a Triple first calls Triple on the number, then calls square on the result of that number.
squareOfTriple :: Num a => a -> a
squareOfTriple number = square (triple number)

--DEFINE a constant for the number of seconds in a week.
--Since the function has a constant, by adding anything else other than the function name means that
--I am processing a parameter, which I am not.
numberOfSecondsInAWeek :: Integer
numberOfSecondsInAWeek = (60 * 60) * 24 * 7

--DEFINE a floating point constnt for the "Golden Ratio"
goldenRatio :: Double
goldenRatio = (1 + sqrt 5) / 2

--DEFINE a Constant for a Mile from KM.  
mile :: Double
mile = 1.609344

--DEFINE function milesToKM :: Double -> Double
milesToKM :: Double -> Double
milesToKM kM = kM * mile 

--DEFINE function kmToMiles :: Double -> Double
kmToMiles :: Double -> Double
kmToMiles miles = miles / mile

--DEFINE a function using existing functions SQUARE and TRIPLE that squares its input and returns the triple of that
squareAndTriple :: Num a => a -> a
squareAndTriple squareAndTripleNumber = triple(square squareAndTripleNumber)

--DEFINE a function that computes the square of the square of its input (the fourth power).
numberToTheFourthPower :: Num a => a -> a
numberToTheFourthPower number = square(square number)

--DEFINE a function with the signature factorial :: Integer -> Integer
-- factorial :: Int -> Int
-- factorial number = product [1..number]

--Integers are variable length numbers which either grow larger or smaller to accomodate the size of the number.  
factorial :: Integer -> Integer
factorial number = product [1..number]

--DEFINE a function with the signature: norm :: Double -> Double -> Double
--Computers the function norm x y = √x^2 + y^2 
norm :: Double -> Double -> Double
norm x y = sqrt (square x + square y)