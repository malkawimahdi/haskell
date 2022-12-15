module TimePastMidnight where

-- Clock shows h hours, m minutes and s seconds after midnight.
-- Your task is to write a function which returns the time since midnight in milliseconds.

-- Example:

-- h = 0
-- m = 1
-- s = 1

-- result = 61000

-- Input constraints:
-- 0 <= h <= 23
-- 0 <= m <= 59
-- 0 <= s <= 59

past :: Int -> Int -> Int -> Int
past hours minutes seconds
    | (hours < 24) && (minutes < 60) && (seconds < 60) = ((hours * 3600) + (minutes * 60) + seconds) * 1000
    | otherwise = 0