module Shared.Inputs.InputPassport (inputPassport) where 

import System.Process (callCommand)
import Shared.Validators.IsNumber
import Shared.Logs.Console
import Data.List.Split    

inputPassport :: String -> IO (Int, Int)
inputPassport infoMessage = do
    putStrLn infoMessage
    putStrLn "Введите серию и номер паспорта (через пробел):"
    input <- getLine
    let parts = splitOn " " input

    if (input == "выход") then return (-1, -1)
    else if length parts /= 2 then do
        callCommand "cls"
        consoleError "Ошибка: должно быть введено два значения (серия и номер)."
        inputPassport infoMessage 
    else do
        let [series, number] = parts
        if not (isNumber series) || length series /= 4 then do
            callCommand "cls"
            consoleError "Ошибка: неверно введена серия паспорта."
            inputPassport infoMessage 
        else if not (isNumber number) || length number /= 6 then do
            callCommand "cls"
            consoleError "Ошибка: неверно введён номер паспорта"
            inputPassport infoMessage 
        else
            return (read series :: Int, read number :: Int)  
