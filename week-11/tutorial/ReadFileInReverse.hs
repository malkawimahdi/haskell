module ReadFileInReverse where

import System.Directory

main :: IO()
main = do
        putStrLn "Please Enter Filename:"
        response <- getLine
        fileContents <- readFile response
        putStrLn $ unlines $ reverse $ lines fileContents