module RepeatUntilEnteredPalindrome where

palindromeCheck :: String -> Bool
palindromeCheck string
    | string == reverse string = True
    | otherwise = False

main :: IO()
main = palindrome 1

palindrome :: Int -> IO()
palindrome attempts = do

        putStrLn "Please Enter a String to Check if it is a Palindrome:"
        response <- getLine
        if response == reverse response then
                putStrLn $ "Is Palindrome " ++ show attempts
        else
            palindrome (attempts + 1)