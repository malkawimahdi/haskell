module CountSheep where

import Text.Printf

countSheep :: Int -> String
countSheep n = concatMap (printf "%d sheep...") [1..n]