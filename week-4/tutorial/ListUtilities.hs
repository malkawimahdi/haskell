module ListUtilities where

import  Data.List

foo :: [a] -> [([a], [a])]
foo xs = zip (inits xs) (tails xs)
bar :: [a] -> [[a]]
bar xs = [ys | ts <- tails xs, ys <- inits ts, not (null ys)]