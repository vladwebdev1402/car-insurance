module Shared.Inputs.InputNumberRegistration (inputNumberRegistration) where 

import System.Process (callCommand)
import Shared.Validators.IsNumber
import Shared.Logs.Console
import Data.Char (toUpper)

validLetters :: String -> Bool
validLetters = all (`elem` "АВЕКМНОРСТУХ")

inputNumberRegistration :: String -> IO (String)
inputNumberRegistration infoMessage = do
    putStrLn "Введите регистрационный номер автомобиля (М999ММ99):"
    input <- getLine
    let upperInput = map toUpper input  
    if length upperInput /= 8 && length upperInput /= 9 then do
        callCommand "cls"
        consoleError "Ошибка: неверная длина строки (8 или 9 символов)."
        inputNumberRegistration infoMessage
    else if not (validLetters (take 1 upperInput)) || not (validLetters (take 2 (drop 4 upperInput))) then do
        callCommand "cls"
        consoleError "Ошибка: в строке содержатся недопустимые символы. Разрешены только: А, В, Е, К, М, Н, О, Р, С, Т, У, Х"
        inputNumberRegistration infoMessage
    else if not (isNumber (take 3 (drop 1 upperInput))) || not (isNumber (take 3 (drop 6 upperInput))) then do
        callCommand "cls"
        consoleError "Ошибка: в строке вместо цифр используются недопустимые символы."
        inputNumberRegistration infoMessage
    else return input
