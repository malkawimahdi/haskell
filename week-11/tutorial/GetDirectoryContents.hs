module DirectoryContents where

import System.Directory
import Data.List

main :: IO()
main = do
        directoryContents <- getDirectoryContents "."
        sequence_ $ [readFile d >>= putStrLn | d <- directoryContents, d /= "." && d /= ".."] 