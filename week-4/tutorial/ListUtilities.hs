module ListUtilities where

import  Data.List

-- listPowerset, takes a list as input and outputs all the possible combinations of the list,
    -- including the empty list.
-- powerset of a list.
listPowerset :: [a] -> [([a], [a])]
listPowerset list = zip (inits list) (tails list)

-- sublists, takes a list as input and outputs all the possible combinations of the list,
    -- NOT including the empty list.
sublists :: [a] -> [[a]]
sublists list = [sublist | tail <- tails list, sublist <- inits tail, not (null sublist)]