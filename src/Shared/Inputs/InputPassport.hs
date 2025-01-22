module Shared.Inputs.InputPassport (inputPassport) where 

import System.Console.ANSI
import Shared.Validators.IsNumber
import Data.List.Split    

inputPassport :: IO (String, String)
inputPassport = do
    putStrLn "Введите серию и номер паспорта (через пробел):"
    input <- getLine
    let parts = splitOn " " input
    if length parts /= 2 then do
        setSGR [SetColor Foreground Vivid Red]
        putStrLn "Ошибка: должно быть введено два значения (серия и номер)."
        setSGR [Reset]
        inputPassport  
    else do
        let [series, number] = parts
        if not (isNumber series) || length series /= 4 then do
            setSGR [SetColor Foreground Vivid Red]
            putStrLn "Ошибка: серия должна содержать 4 цифры."
            setSGR [Reset]
            inputPassport  
        else if not (isNumber number) || length number /= 6 then do
            setSGR [SetColor Foreground Vivid Red]
            putStrLn "Ошибка: номер должен содержать 6 цифр."
            setSGR [Reset]
            inputPassport  
        else
            return (series, number)  
