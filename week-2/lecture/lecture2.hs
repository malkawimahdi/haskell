module Lecture2 where

square :: Integer -> Integer
square number = number * number

norm :: Double -> Double -> Double
norm x y = sqrt (x*x + y*y)

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