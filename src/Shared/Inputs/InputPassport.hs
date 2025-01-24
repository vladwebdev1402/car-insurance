module Shared.Inputs.InputPassport (inputPassport) where 

import System.Process (callCommand)
import Shared.Validators.IsNumber
import Shared.Logs.Console
import Data.List.Split    

inputPassport :: IO (Int, Int)
inputPassport = do
    putStrLn "Введите серию и номер паспорта (через пробел):"
    input <- getLine
    let parts = splitOn " " input
    if length parts /= 2 then do
        callCommand "cls"
        consoleError "Ошибка: должно быть введено два значения (серия и номер)."
        inputPassport  
    else do
        let [series, number] = parts
        if not (isNumber series) || length series /= 4 then do
            callCommand "cls"
            consoleError "Ошибка: неверно введена серия паспорта."
            inputPassport  
        else if not (isNumber number) || length number /= 6 then do
            callCommand "cls"
            consoleError "Ошибка: неверно введён номер паспорта"
            inputPassport  
        else
            return (read series :: Int, read number :: Int)  
