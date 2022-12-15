module OnesAndZeroes where

toNumber :: [Int] -> Int
toNumber numbers = foldl (\n b -> n*n+b ) 0 numbers