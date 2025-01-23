module Shared.Inputs.InputDayOfBirth (inputDayOfBirth) where

import System.Process (callCommand)
import Shared.Inputs.InputDate
import Shared.Calc.CalcAgeFromDate
import Shared.Logs.Console

inputDayOfBirth :: Int -> Int -> IO (Int, String)
inputDayOfBirth minAge maxAge = do
    date <- inputDate
    age <- calcAgeFromDate date
    if age >= minAge && age <= maxAge then return (age, date)
    else do
        callCommand "cls"
        consoleError ("Ошибка: возраст должен быть в диапазоне от " ++ (show minAge) ++ " до "  ++ (show maxAge) ++ " лет.")
        inputDayOfBirth minAge maxAge