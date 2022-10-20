module Week3 where

-- swap takes a pair of ANY TYPE and swaps them,
    -- such that the first element, is the second and vice - versa. 
swap :: (a,b) -> (b, a)
swap pair = (snd pair, fst pair)

-- dup takes a single argument and returns a pair with two copies of the arguement.
dup :: a -> (a, a)
dup anything = (anything, anything)

