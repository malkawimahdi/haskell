{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Week7 where

import Geometry
import Maze
import Search 

import Data.Set (Set)
import qualified Data.Set as Set 
import Data.Map (Map)
import qualified Data.Map as Map

-- unique takes a list as input, with the constraint that the list is ordered and outputs a list with no duplicates.  
-- unique converts the list into a set, then converts the set back to a list. 
unique :: Ord a => [a] -> [a]
unique list = Set.elems $ Set.fromList list 

-- frequencyMap takes a list as input, with the constraint that the list is ordered, and outputs a map, with th number of occurances,
    -- of each element within the input list.
frequencyMap :: Ord a => [a] -> Map a Int
frequencyMap list = Map.fromListWith (+) [(character, 1) | character <- list]  

-- furtherest takes a Maze as input and outputs a set of points, that are most distant from the start point.
furtherest :: Maze -> Set Point
furtherest  (points, start, end) = last $ bfs (moves points) start

-- nearer takes a Maze as input and outputs the set of points that are nearer to the start point than the goal is. 
nearer :: Maze -> Set Point
nearer (points, start, end) = Set.unions $ takeWhile (not .  Set.member end) $ bfs (moves points) start
-- nearer (points, start, end) = Set.unions $ init $ bfs (moves points) start

-- further takes a Maze as input and outputs the set of points that are further then the goal is. 
further :: Maze -> Set Point
further (points, start, end) = Set.unions $ drop 1 $ dropWhile (not .  Set.member end) $ bfs (moves points) start