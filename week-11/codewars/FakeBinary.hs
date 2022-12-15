module FakeBinary where

import Data.Char
-- Given a string of digits, you should replace any digit below 5 with '0' and any digit 5 and above with '1' and 5 with '5'.
    -- Return the resulting string.

-- Note: input will never be an empty string

fakeBinary :: String -> String
fakeBinary [] = []
fakeBinary (x:xs)
    | x < '5' = '0' : fakeBinary xs
    | x > '5' = '1' : fakeBinary xs
    | otherwise = '5' : fakeBinary xs