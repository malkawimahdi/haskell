module InputPalindrome where

main :: IO()
main = do
        putStrLn "Please Enter a String to Check if it is a Palindrome:"
        response <- getLine
        if response == reverse response then
                putStrLn "Is Palindrome"
        else do
            putStrLn "Not Palindrome"
