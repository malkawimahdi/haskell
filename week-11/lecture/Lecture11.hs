module Lecture11 where

main :: IO()
main = do
        s1 <- getLine
        s2 <- getLine
        putStrLn (reverse s1)
        putStrLn (reverse s2)