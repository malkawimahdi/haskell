module Lecture3 where

import Data.Char

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
data Shape
    = Circle Double
    | Rectangle Double Double
    deriving (Eq, Show)

-- Compare
-- compare (2, 'b') (3, 'a') = LT
-- Uses lexigraphical ordering. Compares the first char see's that 2 < 3, and prints LT.   
-- If it is equal, it will compare next pair until it is not equal. If all pairs are deemed equal then output is EQ.

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

    