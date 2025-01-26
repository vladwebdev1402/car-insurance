module Shared.Inputs.InputRangeNumber (inputRangeNumber) where

import System.Process (callCommand)
import Shared.Validators.IsNumber
import Text.Read (readMaybe)
import Shared.Logs.Console

inputRangeNumber :: String -> String -> Int -> Int -> IO Int
inputRangeNumber infoMessage inputMessage minVal maxVal = do
    putStrLn infoMessage
    consoleInfo inputMessage
    input <- getLine
    case readMaybe input of
        Just value | value >= minVal && value <= maxVal -> return value
        _ -> do
            callCommand "cls"
            consoleError ("Ошибка: введите число в диапазоне от " ++ show minVal ++ " до " ++ show maxVal ++ ".")
            inputRangeNumber infoMessage inputMessage minVal maxVal