module RepeatUntilEnteredPalindrome where

palindromeCheck :: String -> Bool
palindromeCheck string
    | string == reverse string = True
    | otherwise = False

main :: IO()
main = do
        putStrLn "Please Enter a String to Check if it is a Palindrome:"
        response <- getLine
        if response == reverse response then
                putStrLn "Is Palindrome"
        else
            main