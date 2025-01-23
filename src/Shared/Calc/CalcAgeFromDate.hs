module Shared.Calc.CalcAgeFromDate (calcAgeFromDate) where 

import Data.Time
import Data.List.Split

calcAgeFromDate :: String -> IO Int
calcAgeFromDate date = do
    currentTime <- getCurrentTime
    let currentDay = utctDay currentTime
    let (currentYear, currentMonth, currentDayOfMonth) = toGregorian currentDay

    let [dayStr, monthStr, yearStr] = splitOn "." date
    let birthDay = read dayStr :: Int
    let birthMonth = read monthStr :: Int
    let birthYear = read yearStr :: Int

    let age =  currentYear - fromIntegral birthYear 

    if (currentMonth > birthMonth) || (currentMonth == birthMonth && currentDayOfMonth >= birthDay)
        then return (fromIntegral age)
        else return (fromIntegral (age - 1) )

