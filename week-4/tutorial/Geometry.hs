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

-- type defines a point as a synonym for an existing type.
data Point = Point Int Int
    deriving (Eq, Ord, Show)

-- Defines an orgion function of type point, to evaluate to a constant. 
origin :: Point
origin = Point 0 0

-- plusPoint takes two points are an arguement and returns a point.
-- plusPoint will use the sum operation on each axis.
    -- each component in the pair, is substituted for the local variables defined in the function.  
plusPoint :: Point -> Point -> Point
plusPoint (Point x1 y1) (Point x2 y2) = Point (x1 + x2)  (y1 + y2)

-- minusPoint takes two points are an arguement and returns a point.
-- minusPoint will use the subtract operation on each axis.
    -- each component in the pair, is substituted for the local variables defined in the function. 
minusPoint :: Point -> Point -> Point
minusPoint (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

-- timesPoint takes two points are an arguement and returns a point.
-- timesPoint will use the multiplication operation on each axis.
    -- each component in the pair, is substituted for the local variables defined in the function and multiplied by factor.
timesPoint :: Int -> Point -> Point
timesPoint factor (Point x1 y1) = Point (x1 * factor) (y1 * factor)

-- normPoint computes the sum of the absolute values of the two componentes.
    -- This is called the Manhattan metic, becuase it represents the minimum distance,
        -- one has to travel to reach the point if one can only move North-South or
            -- East-West on the grid.
normPoint :: Point -> Int
normPoint (Point x1 y1) = x1 + y1

-- distance takes two points and returns an Int, which represents the distance from the two points.
    -- It first subtracts the points, to have a single point and calculates the Manhattan metric on the remaining point.
distance :: Point -> Point -> Int
distance point1 point2 = normPoint (minusPoint point1 point2)

-- oneStep takes as input a Direction and maps the direction to the point one unit from the origin in that direction.
oneStep :: Direction -> Point
oneStep North = Point 0 1
oneStep East = Point 1 0 
oneStep South = Point 0 (-1)
oneStep West = Point (-1) 0

constantPoints :: [Point]
constantPoints = [origin, Point 1 0, Point 2 0, Point 0 (-1),
 Point 1 (-1), Point 2 (-1), Point 0 (-2), Point 1 (-2), Point 2 (-2)]

-- readGrid :: String -> [(Point, Char)]
-- readGrid string = [ (point, char) |
