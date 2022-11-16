module Lecture6 where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

-- smallTest is a maze that is contained as a single string.
-- # WALLS
-- . PASSAGES
-- @ START POINT
-- * GOAL
smallTest :: String
smallTest = "\
    \#########\n\
    \#.#...*.#\n\
    \#.#.#####\n\
    \#.....#.#\n\
    \#.###.#.#\n\
    \#...@...#\n\
    \#########\n"