module Isogram where
    
import Data.Char

isIsogram :: String -> Bool
isIsogram = unique . map toLower
  where unique []     = True
        unique (x:xs) = x `notElem` xs && unique xs