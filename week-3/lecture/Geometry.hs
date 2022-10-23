module Geometry where

-- Direction, is an enumerated type, which represents the cardinal directions.  
data Direction
    = North | East | South | West
    deriving (Show)

-- turnLeft, takes a single Direction as a parameter and returns the corresponding direction, immediately to it's left.
-- Equation defined for possible input and stating the result for each input.
turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft East = North
turnLeft South = East
turnLeft West = South

--turnRight, takes a singled Direction as a parameter and returns the corresponding direction, immediately to it's right.
-- Equation defined for possible input and stating the result for each input. 
turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

-- -- type defines a point as a synonym for an existing type, similar to typedef in C.  
-- type Point = (Int, Int)

-- -- Defines an orgion function of type point, to evaluate to a constant. 
-- origin :: Point
-- origin = (0, 0)

-- -- plusPoint takes two points are an arguement and returns a point.
-- -- plusPoint will use the sum operation on each axis.
--     -- each component in the pair, is substituted for the local variables defined in the function.  
-- plusPoint :: Point -> Point -> Point
-- plusPoint (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Creates a pair with what is to the right of it.
-- It is a parameterised type, whereby it uses A in the type definition.
-- type Pair a = (a, a)

-- Creating a new type called Point, which contains a Pair of Int's. 
-- type Point = Pair Int

-- Defines a new type called "Point" whereby the constructor is called MkPoint which takes two Ints and makes a Point. 
data Point = MkPoint Int Int
    deriving (Show)

-- Defines an orgion function of type point, to evaluate to a constant. 
origin :: Point
origin = MkPoint 0 0

-- plusPoint takes two points are an arguement and returns a point.
-- plusPoint will use the sum operation on each axis.
    -- each component in the pair, is substituted for the local variables defined in the function.  
plusPoint :: Point -> Point -> Point
-- You use the constructor to decompose the input values to its original types. 
-- We use MkPoint to deconstruct the point to it's Int's.
plusPoint (MkPoint x1 y1) (MkPoint x2 y2) = MkPoint (x1+x2) (y1+y2)