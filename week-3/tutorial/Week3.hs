module Week3 where

-- swap takes a pair of ANY TYPE and swaps them,
    -- such that the first element, is the second and vice - versa. 
swap :: (a,b) -> (b, a)
swap pair = (snd pair, fst pair)

-- dup takes a single argument and returns a pair with two copies of the arguement.
dup :: a -> (a, a)
dup anything = (anything, anything)

-- safeDiv, takes two Ints as input and returns a Maybe Int. 
-- This allows us to present the answer with a prefix of Just.
    -- incase of an error, we output Nothing. 
-- This is done to stop a runtime error from occuring. 
--  As noted in session 1, the function div gives a runtime error if its second argument is 0. 
-- This function is a safeguard performing the same thing without a runtime error. 
safeDiv :: Int -> Int -> Maybe Int
safeDiv number1 number2
    | (number2 /= 0) = Just (number1 `div` number2)
    | otherwise = Nothing

-- _ is used to symbolise an anonymous parameter, meaning that you don't care what the parameter is.  
-- pairMaybe, takes a Maybe A, and a Maybe B and if both are Just, then the output is Just,
    -- otherwise the output is Nothing.
pairMaybe :: Maybe a -> Maybe b -> Maybe (a, b)
pairMaybe (Just first) (Just second) = Just (first, second)
pairMaybe _ _ = Nothing

--fromMaybe takes two arguments, a & Maybe a and outputs a if the second parameter is Nothing, otherwise the first parameter.
fromMaybe :: a -> Maybe a -> a
fromMaybe first (Just second) = second
fromMaybe first _ = first

-- Takes a single parameter and strips the Either segment.
whatever :: Either a a -> a
whatever (Left a) = a
whatever (Right b) = b

-- new Type, that contains either OK and a type of an Error with a String. 
data Err a = OK a | Error String
    deriving (Show)

-- Takes two Errs, and outputs Err in a tuple if both are OK or outputs and Error message, with both being concatenated.
both :: Err a -> Err b -> Err (a, b)
both (OK a) (OK b) = OK (a,b)
both (Error msg1) (Error msg2) = Error (msg1 ++ "/n" ++ msg2)
