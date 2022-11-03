module Lecture5 where

import Data.Char
-- Higher order functions allow us to avoid repitition and write more powerful programs.
-- They also allow us to wrap up common functionality.  

-- ordAll takes a list of Chars and returns a list of Ints,
    -- whereby each char is taken out of the list iteratively and replaced with the ASCII equivalent.
-- ordAll :: [Char] -> [Int]
-- ordAll chars = [ord char | char <- chars]

-- pickEven takes a list of Ints and returns a list of Ints,
    -- whereby each Int is taken out of the list iteratively and checked against the constraints 
        -- applied in the list comprehension.
-- pickEven :: [Int] -> [Int]
-- pickEven numbers = [number | number <- numbers, even number]

-- letters takes a list of Chars and returns a list of Chars,
    -- whereby each char is taken out of the list iteratively and checked against the constraint,
        -- to determine if the character is alpha numerical. The output list will only contain letters.
-- letters :: [Char] -> [Char]
-- letters chars = [char | char <- chars, isAlpha char]

-- ordAll takes a list of Chars and returns a list of Ints,
    -- whereby each char is taken out of the list iteratively and replaced with the ASCII equivalent.
-- Equivalent to what is produced above, however this implementation uses a higher order function called "map"
    -- that allows us to provide the function as an argument, called ord to apply to the list of Char.
ordAll:: [Char] -> [Int]
ordAll chars = map ord chars

-- pickEven takes a list of Ints and returns a list of Ints,
    -- Uses a higher order function called filter, in which the first argument is a function,
        -- the second argument is a list.
pickEven :: [Int] -> [Int]
pickEven numbers = filter even numbers

-- letters takes a list of Chars and returns a list of Chars,
    -- Uses a higher order function called filter, in which the first argument is a function,
        -- the second argument is a list.
letters :: [Char] -> [Char]
letters chars = filter isAlpha chars