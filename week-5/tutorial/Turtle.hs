module Turtle where

import Geometry
import Generator

-- state of a turtle
data Turtle = Turtle Point Direction PenState
    deriving Show

-- state of a turtle's pen
data PenState = PenUp | PenDown
    deriving Show

-- start at the origin, facing North, with pen up
startTurtle :: Turtle
startTurtle = Turtle origin North PenUp

-- location of the turtle
location :: Turtle -> Point
location (Turtle pos dir pen) = pos

-- a command for a turtle
data Command
    = TurnLeft | TurnRight | Move Int | RaisePen | LowerPen
    deriving Show

-- action of a turtle command
action :: Turtle -> Command -> Turtle
action (Turtle pos dir pen) TurnLeft =
    Turtle pos (turnLeft dir) pen
action (Turtle pos dir pen) TurnRight =
    Turtle pos (turnRight dir) pen
action (Turtle pos dir pen) (Move n) =
    Turtle (plusPoint pos (timesPoint n (oneStep dir))) dir pen
action (Turtle pos dir _) RaisePen =
    Turtle pos dir PenUp
action (Turtle pos dir _) LowerPen =
    Turtle pos dir PenDown

-- convert a number into a command
genCommand :: Int -> Command
genCommand n
  | r == 0 = TurnLeft
  | r == 1 = TurnRight
  | r == 2 = RaisePen
  | r == 3 = LowerPen
  | otherwise = Move (r - 3)
    where
    r = n `mod` 11

-- generate an infinite list of commands, for testing
genCommands :: Int -> [Command]
genCommands seed = map genCommand (generate seed)

-- How many steps does the turtle need to reach 400?
-- length $ takeWhile (< 400) $  map normPoint $ map location $ scanl action startTurtle $ genCommands 37