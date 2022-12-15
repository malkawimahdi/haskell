module RemoveExclamationMarks where

-- Write function RemoveExclamationMarks which removes all exclamation marks from a given string.

removeExclamationMarks :: String -> String
removeExclamationMarks [] = []
removeExclamationMarks (x:xs)
    | x == '!' = removeExclamationMarks xs
    | otherwise = x : removeExclamationMarks xs