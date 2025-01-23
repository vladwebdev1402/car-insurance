module Shared.Inputs.InputRangeNumber (inputRangeNumber) where

import System.Process (callCommand)
import Shared.Validators.IsNumber
import Shared.Logs.Console

inputRangeNumber :: String -> String -> Int -> Int -> IO Int
inputRangeNumber infoMessage inputMessage minVal maxVal = do
    putStrLn $ infoMessage
    consoleInfo inputMessage
    input <- getLine
    case isNumber input && read input >= minVal && read input <= maxVal
        of
            True -> return (read input)
            False -> do
                callCommand "cls"
                consoleError ("Ошибка: введите число в диапазоне от " ++ (show minVal) ++ " до " ++ (show maxVal) ++ ".")
                inputRangeNumber infoMessage inputMessage minVal maxVal