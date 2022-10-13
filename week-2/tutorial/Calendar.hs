module Calendar where
import Data.Time.Calendar.Month (Month)

-- In the Gregorian calendar, a year is a leap year if it is divisible by 4,
    -- except that centuaries are leap years only if divisble by 400.
-- Thus, 2000 was a leap year, but 2100 won't be.  
-- isLeapYear takes a year as a parameter and returns True or False, depending if it is a Leap Year.  
isLeapYear :: Int -> Bool
isLeapYear year
    | year `mod` 400 == 0 = True
    | year `mod` 100 == 0 = False
    | year `mod` 4 == 0   = True
    | otherwise           = False

-- Days within a year vary if the year is a leap year or not. 
-- If the year is a leap year, then it will have 366 days in year otherwise it is 365.
-- daysInYear takes a year as Int as input and outputs an Int.  
daysInYear :: Int -> Int
daysInYear year
    | isLeapYear year = 366
    | otherwise       = 365

-- Month, is an enumerated type, which represents the months in any given year. 
data Month
    = January | February | March | April | May | June | July | August | September | October | November | December
    deriving (Show)