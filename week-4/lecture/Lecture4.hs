module Week4 where

-- LISTS AND INFINITE LISTS --

-- Everything within a list have to have the same type.

-- Can't modify a value contained within a list. 

-- Any type with square brackets around them such as [Int] denote that it is a list of the type.

-- You do not have to declare the length of the lists in Haskell. 

-- Strings are lists of characters in Haskell.

-- A list can contain ANYTHING that is OF THE SAME type. E.g. ["Hello","world""] :: [[Char]]. 
                                                           --  [div, (+), (*)] :: [Int -> Int -> Int]
                                                           -- "" :: [Char]
                                                           -- [] :: [a]

-- Any list is greater than the empty list. compare "a" "" = GT. Uses lexigraphical comparison to determine the properties of LT, EQ, GT.

-- Strings are another name for [Char], so String = [Char].  

-- Infinite lists turn out to be useful as long as the operations you perform on the lists are performed at the start. 
-- It is NOT useful if you want to try to calculate the length of an infinite list. The operations you perform should be 
    -- used to operate at the start of an infinite list.  E.g. take 10 [1..]

-- For the infinite list of even numbers: [0, 2..] 
-- For the infinite list of odd numbers: [1, 3..]
-- For the infinite list of negative numbers: [-1, -2..]

-- As you can see, the pattern of a list, can be determined by the stepping used.

-- LIST COMPREHENSIONS --

-- Haskell has a similar syntax to set comprehensions. 
-- Sets are not the same as lists as Haskell allows duplications and ordering.

-- [f x y | x <- xs, y <- ys, x + y <= 5]

-- [number * number | number <- [1..10]] (Squares the numbers from the list).   

-- [ (x,c) | x <- [1..3], c <- "abcd"] (Cartesian Products).

-- [ (x, y) | x <- [1..4], y <- [1..3]] (Cartesian Products).

-- [ (x, y) | x <- [1..4], y <- [1..3], x+y <= 5] (Example of conditional guard).

-- [ x * y | x <- [1..4], y <- [1..3], x+y <= 5] (Example of conditional guard).

-- [ c | c <- "Hello World"] (Example of conditional guard).

-- [c | c <- "Hello World", isLower c] (Example of outputing the lower case letters only).

-- Lists CAN CONTAIN REPITITIONS. 


