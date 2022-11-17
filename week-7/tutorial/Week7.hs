{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Week7 where

import Geometry
import Maze

import Data.Set (Set)
import qualified Data.Set as Set 
import Data.Map (Map)
import qualified Data.Map as Map
import Search (bfs)



-- unique takes a list as input, with the constraint that the list is ordered and outputs a list with no duplicates.  
-- unique converts the list into a set, then converts the set back to a list. 
unique :: Ord a => [a] -> [a]
unique list = Set.elems $ Set.fromList list 

--frequencyMap takes a list as input, with the constraint that the list is ordered, and outputs a map, with th number of occurances,
    -- of each element within the input list.
frequencyMap :: Ord a => [a] -> Map a Int
frequencyMap list = Map.fromListWith (+) [(character, 1) | character <- list]  

furtherest :: Maze -> Set Point
furtherest  (points, start, end) = last $ bfs (moves points) start

