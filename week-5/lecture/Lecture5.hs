module Lecture5 where

import Data.Char
-- Higher order functions allow us to avoid repitition and write more powerful programs.  

ordAll :: [Char] -> [Int]
ordAll chars = [ord char | char <- chars]

pickEven :: [Int] -> [Int]
pickEven numbers = [number | number <- numbers, even number]

