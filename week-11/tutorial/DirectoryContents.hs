module DirectoryContents where

import System.Directory
import Data.List

main :: IO()
main = do
        directoryContents <- getDirectoryContents "."
        putStr $ unlines [d| d <- directoryContents, d /= "." && d /= ".."] 

