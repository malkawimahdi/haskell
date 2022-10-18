module Lecture3 where

import Data.Char

-- Direction, is an enumerated type, which represents the cardinal directions.  
data Direction
    = North | East | South | West
    deriving (Show)


data Shape
    = Circle Double
    | Rectangle Double Double
    deriving (Eq, Show)

area :: Shape -> Double
area (Circle radius) = pi * radius * radius
area (Rectangle height width) = height * width

scale :: Double -> Shape -> Shape
scale factor (Circle radius) = Circle (factor * radius)
scale factor (Rectangle height width) = Rectangle (factor * height) (factor * width)

rotate :: Shape -> Shape
rotate (Circle radius) = Circle radius
rotate (Rectangle height width) = Rectangle width height

charToNum :: Char -> Maybe Int
charToNum character
    | isDigit character = Just (ord character - ord '0')
    | otherwise = Nothing

    