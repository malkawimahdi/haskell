module AreaPerimeter where

--You are given the length and width of a 4-sided polygon. The polygon can either be a rectangle or a square.
    -- If it is a square, return its area. If it is a rectangle, return its perimeter.

-- Example(Input1, Input2 --> Output):
-- 6, 10 --> 32
-- 3, 3 --> 9

areaOrPerimeter :: Double -> Double -> Double
areaOrPerimeter number1 number2
    | number1 == number2 = number1 * number2
    | otherwise = (number1 * 2) + (number2 * 2)