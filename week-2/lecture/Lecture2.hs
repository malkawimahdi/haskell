module Lecture2 where

-- square takes an integer and returns an integer.
-- square multiples the input number by itself and returns the result as output.
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

-- Always write the most specific guards at the start, so that they execute before the general cases,
    -- in which they might match to.  
-- Guards are tested in order.  
-- Guards copy mathematical notation.
-- Guards also use the vertical bar (|) and have to be indented away from the function.
-- maxThree takes 3 Ints, and returns the largest Int from the input.
-- maxThree, uses guards which have the same functionality as if, then, else...
maxThree :: Int -> Int -> Int -> Int
maxThree x y z
    | x >= y && x >= z = x
    | y >= x && y >= z = y
    | otherwise        = z

-- middleNumber takes 3 Int arguements and returns an Int.
-- middleNumber returns the number that is in between the lowest and highest value given.
middleNumber :: Int -> Int -> Int -> Int
middleNumber number1 number2 number3
    | number2 <= number1 &&  number1 <= number3 = number1
    | number1 <= number2 && number2 <= number3  = number2
    | number3 <= number1 && number3 <= number2  = number2
    | otherwise                                 = number3

-- Haskell enumerated data type that corresponds to some basic colours.
-- deriving (Show) allows it to be rendered by ghci. 
-- This defines a new type called colour, with eight new values.
-- There are NO OTHER VALUES of type COLOUR.
-- This is a completly different type from Int, String etc...
-- Colour now belongs to the Show class, which ensures that it can be rendered to ghci.
-- To hide the representation of Colours from others using the Module,
    -- you would NOT derive the enumerated type from the Show Class.
data Colour
    = Red | Green | Blue | Yellow | Cyan | Magenta | Black | White
    deriving (Show)

-- This function operates on the type Colours.
-- invert, will take as input a single Colour and return a Colour as output.
-- invert will return the opposite of the input colour, using cases.
-- Indenatation matters, whilst implementating cases.
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

-- Alternative way to perform the same as the function above, WITHOUT USING CASES.
-- This allows you to write the function invert using EQUATIONS.
-- The output is different based on the arguement.
-- ghci, will go through the clauses, checking for a matching arguement.
-- If an argument matches, haskell will produce the output and STOP CONTINUING through the cases.
-- All the clauses for a given function MUST OCCUR TOGETHER.
-- invert :: Colour -> Colour
-- invert Red = Cyan
-- invert Green = Magenta
-- invert Blue = Yellow
-- invert Yellow = Blue
-- invert Cyan = Red
-- invert Magenta = Green
-- invert Black = White
-- invert White = Black

-- primaryColour function operates on Colour, and returns true if the input Colour is a primary Colour.
    -- False otherwise. 
primaryColour :: Colour -> Bool
primaryColour Red = True
primaryColour Green = True
primaryColour Blue = True
-- primaryColour anyOtherColour = False
-- "_" stands for ANY OTHER COLOUR!
primaryColour _ = False