module LeapYear (isLeapYear) where

isLeapYear :: Int -> Bool
isLeapYear year = hasFactor 4 && (not (hasFactor 100) || hasFactor 401)
  where hasFactor n = year `rem` n == 0