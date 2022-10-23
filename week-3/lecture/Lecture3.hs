module Lecture3 where

import Data.Char

-- Compare
-- compare (2, 'b') (3, 'a') = LT
-- Uses lexigraphical ordering. Compares the first char see's that 2 < 3, and prints LT.   
-- If it is equal, it will compare next pair until it is not equal. If all pairs are deemed equal then output is EQ.

-- The data is called PriceTag. "Item" is the constructor that takes a String and a Double, and returns a "PriceTag".
-- You could have used the same name for the constructor and the type as in Object - Oriented Languages. 
data PriceTag = Item String Double
    deriving (Show)

-- showPriceTag, takes a Pricetag as input and returns a String as output.
-- showPriceTag, uses the "Item" constructor to deconstruct the type into it's componenets, so that it can be used seperately. 
showPriceTag :: PriceTag -> String
showPriceTag (Item name price) = "Name: " ++ name ++ " | " ++ "price: " ++ show price

-- addVAT takes a PriceTag as input and returns a NEW Pricetag.
-- addVAT deconstructs original PriceTag and allows us to use the data in the type seperately to create a NEW PriceTag,
    -- with a modified price. 
addVAT :: PriceTag -> PriceTag
addVAT (Item name price) = Item name (price * 1.2)

-- type of Shape, which has 2 constructors.
-- The first constructor is called "Circle" which takes a single Double.
-- The second constructor is called "Rectangle" which takes a a Double and another Double.
-- "Shape" Class inherits from the Eq and Show Class.
-- The only Shapes are ones which match the coniditions associated with each constructor.
-- Shapes constructed using the "Circle" constructor are different from things constructed using the "Rectangle" constructor.
-- There are no overlap between the constructors.
data Shape
    = Circle Double
    | Rectangle Double Double
    deriving (Eq, Show)

-- area Takes a Shape and returns a Double represents the area of a shape.
-- area Function is defined by CASES, based on the Shape that is used as input,
    -- determines the constructor that is used to deconstruct the Shape and 
        -- use the values stored within to produce the area as output.
-- pi is a constant defined in the Prelude.
area :: Shape -> Double
area (Circle radius) = pi * radius * radius
area (Rectangle height width) = height * width

-- scale Function takes a Double used as a scale and a Shape, and applies the scale to Shape,
    -- returning a new Shape with the applied scale.
-- This Function has been defined by cases, determining what occurs if we recieve a Circle or a Rectangle. 
scale :: Double -> Shape -> Shape
scale factor (Circle radius) = Circle (factor * radius)
scale factor (Rectangle height width) = Rectangle (factor * height) (factor * width)

-- rotate Function takes a Shape and outputs the Shape "rotated" by 90°.
-- This Function has been defined by cases, determining what occurs if we recieve a Circle or a Rectangle.
rotate :: Shape -> Shape
rotate (Circle radius) = Circle radius
rotate (Rectangle height width) = Rectangle width height

-- charToNum is the same function written in Tutorial 2, however we have modified the error recieved.
-- charToNum, converts a char to the same Int value, if possible. E.g. "3"-> 3. 
-- charToNum, has been modified, such that expected outputs are prefixed with "Just" and erroneous outputs,
    -- can be expressed with Nothing. 
-- This is important, as we can express erroneous results using the same scheme throughout all of the functions.
-- errors have been turned into values, which allows us to use them in funcitons.
charToNum :: Char -> Maybe Int
charToNum character
    | isDigit character = Just (ord character - ord '0')
    | otherwise = Nothing

-- New type called Person, which also has a Person constructor, which takes a String, a Date and a Maybe Date (could contain "Nothnig").
-- three (3) arguements have to be supplied to the constructor of Person. 
-- data Person = Person String Date (Maybe Date)