module RiverCrossing where

import Search

import Data.Set (Set)
import qualified Data.Set as Set 

type Farmer = String
type Wolf = String
type Goat = String
type Cabbage = String

data State = State Farmer Wolf Goat Cabbage
    deriving (Eq, Ord, Show)

