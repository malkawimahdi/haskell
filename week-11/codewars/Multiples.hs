module Multiples where

multiples :: Int -> Int -> [Int]
multiples startNumber limitNumber = [number | number <- [startNumber,startNumber*2..limitNumber], number <= limitNumber]