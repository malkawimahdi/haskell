-- a) frequencies :: [(String, Int)]
-- is a list of words with the number of times they occur in some document.
-- (You may assume that there is only one entry in the list for each distinct
-- word.) Give expressions for the following:

-- i) the number of different words in the original document.
-- length (frequency string)

-- ii) The total number of words in the original document.
-- sum [number | (string, number) <- frequency string]

-- iii) The number of words that occur exactly once each in the original document.
-- sum [number | (string, number) <- frequency string, number == 1]


-- b) Consider the function
-- foo :: Eq a => a -> [a] -> [Int]
-- foo x ys = [n | (n, y) <- zip [1..] ys, x == y]

-- i) Explain why the Eq constraint on the first line is required.
-- The equality contraint is required in the function definition as the function
    -- in the second line uses the equality function (==) which requires that 
        -- that the elements compared are of a type that can be ordered in some sense.

-- ii) Give the value of foo ’a’ "abracadabra".
-- This function will return all the positions of where the "x" value occurs.
-- [1,4,6,8,11]

-- iii) In general, how is the list returned by foo related to its argument?
-- This function will return all the positions of where the "x" value occurs.


-- c) Consider the following function definition:
-- bar :: Ord a => a -> [a] -> [a]
-- bar x [] = [x]
-- bar x (y:ys)
-- | x > y = y : bar x ys
-- | otherwise = x : y : ys

-- i) Give the value of the expression bar 7 [5,3].
-- [5,3,7]

-- ii) Give the value of the expression bar 4 [1,6,2,7].
-- [1,4,6,2,7]

-- iii) In general, how is the list returned by bar related to its argument?
-- bar checks if the input a is greater than everything within the list of a's. 
    -- If true the list is retuned, Otherwise the function will place x, where the condition has failed. 

-- iv) Define an equivalent function, but without using recursion.
