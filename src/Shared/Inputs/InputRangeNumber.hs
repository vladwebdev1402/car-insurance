module Shared.Inputs.InputRangeNumber (inputRangeNumber) where

import System.Console.ANSI
import System.Process (callCommand)
import Shared.Validators.IsNumber

inputRangeNumber :: String -> String -> Int -> Int -> IO Int
inputRangeNumber infoMessage inputMessage minVal maxVal = do
    putStrLn $ infoMessage
    putStrLn $ inputMessage
    input <- getLine
    case isNumber input && read input >= minVal && read input <= maxVal
        of
            True -> return (read input)
            False -> do
                callCommand "cls" 
                setSGR [SetColor Foreground Vivid Red]
                putStrLn $ "Ошибка: введите число в диапазоне от " ++ (show minVal) ++ " до " ++ (show maxVal) ++ "."
                setSGR [Reset]
                inputRangeNumber infoMessage inputMessage minVal maxVal