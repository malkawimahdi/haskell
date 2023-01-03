module Question2 where

import Data.Char

-- a) Write a function
-- letters :: String -> String
-- that returns the original string with all non-letters removed

-- b) Using the function letters from the previous part, and the library functions getLine and putStrLn, write a program fragment to read a line from
-- the console and print a line consisting of the letters in that line. 
letters :: String -> String
letters (c:cs) = [c | c <-(c:cs), isAlpha c]

main :: IO()
main = 
    do
        inputLine <- getLine 
        putStrLn (letters inputLine)

-- c) Consider the function
-- mystery [] = []
-- mystery (x:xs) = xs ++ [x]

-- i) Give a type signature for mystery.
-- mystery :: [a] -> [a]

-- ii) Give the value of mystery [4,5,6].
-- Output: [5,6,4]

-- iii) In general, how is the value returned by mystery related to its argument?
-- mystery takes the input value and will append the value to the end of the existing list.