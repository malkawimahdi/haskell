module Geometry where

-- Direction, is an enumerated type, which represents the cardinal directions.  
data Direction
    = North | East | South | West
    deriving (Show)

-- turnLeft, takes a single Direction as a parameter and returns the corresponding direction, immediately to it's left.
turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft East = North
turnLeft South = East
turnLeft West = South

--turnRight, takes a singled Direction as a parameter and returns the corresponding direction, immediately to it's right.
turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

-- -- type defines a point as a synonym for an existing type.
-- type Point = (Int, Int)

-- -- Defines an orgion function of type point, to evaluate to a constant. 
-- origin :: Point
-- origin = (0, 0)

-- -- plusPoint takes two points are an arguement and returns a point.
-- -- plusPoint will use the sum operation on each axis.
--     -- each component in the pair, is substituted for the local variables defined in the function.  
-- plusPoint :: Point -> Point -> Point
-- plusPoint (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


data Point = MkPoint Int Int
    deriving (Show)

-- Defines an orgion function of type point, to evaluate to a constant. 
origin :: Point
origin = MkPoint 0 0

-- plusPoint takes two points are an arguement and returns a point.
-- plusPoint will use the sum operation on each axis.
    -- each component in the pair, is substituted for the local variables defined in the function.  
plusPoint :: Point -> Point -> Point
plusPoint (MkPoint x1 y1) (MkPoint x2 y2) =
    MkPoint (x1+x2) (y1+y2)

-- type names start with a Capital letter.
type Pair a = (a,a)

-- The data is called PriceTag. Item is the constructor that takes a String and a Double, and returns a PriceTag. 
data PriceTag = Item String Double
    deriving (Show)

