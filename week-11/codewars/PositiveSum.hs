module PositiveSum where

-- You get an array of numbers, return the sum of all of the positives ones.
-- Example [1,-4,7,12] => 1 + 7 + 12 = 20
-- Note: if there is nothing to sum, the sum is default to 0.

isNegative :: Int -> Bool
isNegative number
    | number < 0 = True
    | otherwise = False

positiveSum :: [Int] -> Int
positiveSum numbers = foldl (+) 0 positiveNumbers
    where positiveNumbers = filter (not . isNegative) numbers