{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Turtle where

import Geometry

-- TurtlePen, is an enumerated type, whereby it is either Up or Down.
data TurtlePen = Up | Down
    deriving (Show)

-- Turtle type, whereby the constructor for this type,
    -- takes a Point, a Direction and an enumerated type Up or Down. 
data Turtle = Turtle Point Direction TurtlePen
    deriving (Show)

-- startTurtle takes no arguements, and produces an output
    -- with a starting configuration whereby the Turtle starts at the origin,
        -- the Direction is north and the TurtlePen is Up. 
startTurtle :: Turtle
startTurtle = Turtle origin North Up

-- location takes a single parameter and returns a point. 
    -- It takes the input, and splits the input, by using the constructor to only output the point stored.  
location :: Turtle -> Point
location (Turtle point direction turtlePen) = point

-- Command type, has five(5) constructors, whereby 4 have nothing. Steps, contains an int. 
data Command = TurnLeft | TurnRight | Steps Int | LiftPen | LowerPen
    deriving (Show)

-- action takes a Turtle and a command and returns a Turtle, with the new values. 
action :: Turtle -> Command -> Turtle
action (Turtle point direction turtlePen) TurnLeft = Turtle point (turnLeft direction) turtlePen
action (Turtle point direction turtlePen) TurnRight = Turtle point (turnRight direction) turtlePen
action (Turtle point direction turtlePen) steps = Turtle point (oneStep direction) turtlePen
action (Turtle point direction turtlePen) LiftPen = Turtle point (turnRight direction) Up
action (Turtle point direction turtlePen) LowerPen = Turtle point (turnRight direction) Down