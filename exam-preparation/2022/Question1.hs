module Question1 where

import Data.Char
import System.Directory
import Data.List

-- Given the definition:
powers :: [Integer]
powers = iterate (*2) 1

-- i) The sum of the first n elements of powers
--  sum $ take n

-- ii) The number of elements of powers less than n.
-- length takeWhile (<n) (powers list)

-- iii) The sum of elements of powers less than n.
-- sum $ takeWhile (<n) (powers list)

-- iv) The list of elements of powers between m and n (inclusive).
---- takeWhile (\>=m || <=n) (powers list)

-- b) Define a function that returns all combinations of pairs of elements from two lists.
combinations :: [a] -> [b] -> [(a,b)]
combinations list1 list2 = [(a,b) | a <- list1, b <- list2]

-- c) Give a defintion of a function that capitalisaes the first letter of each workd in a string. 
-- You may assume words are seperated by spaces.
capitalize :: String -> String
capitalize s = unwords [toUpper c : cs | (c:cs) <- words s]

-- d)  Using the library functions readFile and putStr, write a program fragment to read the contents of a file in.txt and 
    -- print to the console the lines of the file, each preceded by its line number.
main :: IO()
main = 
    do
        file <- readFile "test.txt"
        putStr $ unlines [show number ++ " " ++ line | (number, line) <- zip [1..] (lines file)]
