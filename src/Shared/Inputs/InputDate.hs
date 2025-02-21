module Shared.Inputs.InputDate (inputDate) where 

import System.Process (callCommand)
import Shared.Logs.Console
import Shared.Validators.IsNumber
import Data.List.Split    

validateDay :: Int -> Int -> Int -> Bool
validateDay day month year
    | day < 1 || day > 31 = False  
    | month `elem` [4, 6, 9, 11] && day > 30 = False 
    | month == 2 = if isLeapYear year then day <= 29 else day <= 28 
    | otherwise = True  

isLeapYear :: Int -> Bool
isLeapYear year
    | year `mod` 400 == 0 = True
    | year `mod` 100 == 0 = False
    | year `mod` 4 == 0 = True
    | otherwise = False

validateMonth :: Int -> Bool
validateMonth month 
    | month < 1 || month > 12 = False 
    | otherwise = True

validateYear :: Int -> Bool
validateYear year = year >= 1920

inputDate :: IO String
inputDate = do
    putStrLn "Введите дату рождения через точку (.) в формате ДД.ММ.ГГГГ:"
    input <- getLine
    let parts = splitOn "." input
    if length parts /= 3 then do
        callCommand "clear"
        consoleError "Ошибка: должно быть введено три значения (день, месяц, год)."
        inputDate  
    else do
        let [day, month, year] = parts
        let dayNum = read day :: Int
        let monthNum = read month :: Int
        let yearNum = read year :: Int

        if not (isNumber day) || length day /= 2 then do
            callCommand "clear"
            consoleError "Ошибка: день должен содержать 2 цифры (например: 01, 14, 31)."
            inputDate  
        else if not (isNumber month) || length month /= 2 then do
            callCommand "clear"
            consoleError "Ошибка: месяц должен содержать 2 цифры (например: 01, 02, 10)."
            inputDate  
        else if not (isNumber year) || length year /= 4 then do
            callCommand "clear"
            consoleError "Ошибка: год должен содержать 4 цифры. (например: 2003, 1964, 2000)."
            inputDate  
        else if not (validateYear yearNum) then do
            callCommand "clear"
            consoleError "Ошибка: год введён неверно"
            inputDate  
        else if not (validateMonth monthNum) then do
            callCommand "clear"
            consoleError "Ошибка: месяц введён неверно"
            inputDate  
        else if not (validateDay dayNum monthNum yearNum) then do
            callCommand "clear"
            consoleError "Ошибка: день введён неверно"
            inputDate  
        else do return (day ++ "." ++ month ++ "." ++ year)