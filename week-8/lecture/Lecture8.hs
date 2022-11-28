module Lecture8 where

-- lastListElement taks a list and outputs a list containing the last element.
lastListElement :: [a] -> [a]
lastListElement [] = []
lastListElement  (x:xs)
    | length xs == 1 = xs
    | otherwise = lastListElement xs