module Shared.Inputs.InputFio (inputFio) where 

import System.Process (callCommand)
import Shared.Logs.Console
import Data.List.Split  
import Data.Char (isLetter, isDigit)

isValidNamePart :: String -> Bool
isValidNamePart name =
  all (\c -> isLetter c || c == '-') name && 
  length name > 2 &&                         
  not (any isDigit name)                     

inputFio :: IO (String)
inputFio = do
    putStrLn "Введите ФИО:"
    input <- getLine
    let parts = splitOn " " input
    if length parts /= 3 then do
        consoleError "Ошибка: должно быть введено три значения (фамилия, имя, отчество)."
        inputFio  
    else do
        let [surName, firstName, patroName] = parts

        if not (isValidNamePart surName) then do  
            callCommand "clear"
            consoleError "Ошибка: фамилия введена неверно."
            inputFio
        else if not (isValidNamePart firstName) then do  
            callCommand "clear"
            consoleError "Ошибка: имя введено неверно."
            inputFio
        else if not (isValidNamePart patroName) then do  
            callCommand "clear"
            consoleError "Ошибка: отчество введено неверно."
            inputFio
        else return (surName ++ " " ++ firstName ++ " " ++ patroName)