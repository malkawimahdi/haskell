module Turtle where

import Geometry

data Turtle = Turtle Point Direction Bool
    deriving (Show)