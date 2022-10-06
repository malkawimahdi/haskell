import Distribution.Simple.Program.HcPkg (list)
--greet is defined as a function that takes no arguements and returns a CONSTANT string called "Hello World".
greet :: String
greet = "hello world!"

--Write a function that takes an array of numbers and returns the sum of the numbers.
--The numbers can be negative or non-integer. 
--If the array does not contain any numbers then you should return 0.
sumCustom :: Num a => [a] -> a 
sumCustom  = Prelude.sum


--Simple, remove the spaces from the string, then return the resultant string.
noSpace :: String  -> String
noSpace = concat.words