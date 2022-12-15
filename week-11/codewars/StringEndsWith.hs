module StringsEndsWith where

import Data.List

stringsEndsWith :: String -> String -> Bool
stringsEndsWith x y = isSuffixOf y x