module Geometry where

-- Direction, is an enumerated type, which represents the cardinal directions.  
data Direction
    = North | East | South | West
    deriving (Show)

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft East = North
turnLeft South = East
turnLeft West = South

turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North