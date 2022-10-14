module Lecture2 where

square :: Integer -> Integer
square number = number * number

-- norm is a function that takes 2 Doubles and outputs a Double.
-- Any negative input will become positive.  
norm :: Double -> Double -> Double
norm x y = sqrt (x*x + y*y)

-- small function takes a single Int and outputs a Bool. 
-- This function is able to conclude if a number is between 0 - 9,
    -- outputting the corresponding boolean expression.  
small :: Int -> Bool
small number = 0 <= number && number < 10

-- Haskell enumerated data type that corresponds to some basic colours.
-- deriving (Show) allows it to be displayed within ghci.  
data Colour
    = Red | Green | Blue | Yellow
    | Cyan | Magenta | Black | White
    deriving (Show)

invert :: Colour -> Colour
invert colour = case colour of
    Red -> Cyan
    Green -> Magenta
    Blue -> Yellow
    Yellow -> Blue
    Cyan -> Red
    Magenta -> Green
    Black -> White
    White -> Black

-- invert :: Colour -> Colour
-- invert Red = Cyan
-- invert Green = Magenta
-- invert Blue = Yellow
-- invert Yellow = Blue
-- invert Cyan = Red
-- invert Magenta = Green
-- invert Black = White
-- invert White = Black

primaryColour :: Colour -> Bool
primaryColour Red = True
primaryColour Green = True
primaryColour Blue = True
-- primaryColour anyOtherColour = False
primaryColour _ = False

-- Always write the most specific guards at the start, so that they execute before the general cases,
    -- in which they might match to.  
