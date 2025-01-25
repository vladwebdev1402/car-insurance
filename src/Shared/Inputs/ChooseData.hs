module Shared.Inputs.ChooseData (chooseData, choosePaginatedData, chooseApiPaginatedData, chooseApiParamPaginatedData) where

import System.Console.ANSI
import System.Process (callCommand)
import Data.List.Split 
import Data.List
import Shared.Validators.ValidateNumberRangeInput
import Shared.Logs.LogData
import Shared.Logs.Console

chooseData :: [a] -> ([a] -> IO ()) -> String -> String -> IO Int
chooseData arrayData displayFunction inputMessage infoMessage = do
    callCommand "cls"
    putStrLn infoMessage
    consoleInfo inputMessage
    displayFunction arrayData
    input <- getLine
    if (input == "выход") then return (-1)
    else case validateNumberRangeInput 1 (length arrayData) input of
            Just num -> return num
            Nothing -> do
                callCommand "cls" 
                consoleError ("Ошибка: введите число от 1 до " ++ show (length arrayData) ++ ".")
                chooseData arrayData displayFunction inputMessage infoMessage

choosePaginatedData :: [a] -> ([a] -> IO ()) -> IO (Int, String)
choosePaginatedData arrayData displayFunction = do
    displayFunction arrayData
    input <- getLine
    case validateNumberRangeInput 1 (length arrayData) input of
        Just num -> return (num, "")
        Nothing -> do
            case input of
                "назад" -> return (-1, "назад")
                "вперёд" -> return (-1, "вперёд")
                "сбросить" -> return (-1, "сбросить")
                _ -> do
                    if "поиск" `isInfixOf` input then do
                        let parts = splitOn " " input
                        if length parts /= 2 then do
                            callCommand "cls" 
                            consoleError ("Введите название для поиска")
                            choosePaginatedData arrayData displayFunction
                        else do
                            let [_, name] = parts
                            return (-1, name)
                    else do
                        callCommand "cls" 
                        consoleError ("Ошибка: введите число от 1 до " ++ show (length arrayData) ++ ".")
                        choosePaginatedData arrayData displayFunction

-- страница, поисковое имя, функция запроса, куда передаётся максимальный и минимальный индекс, функция для получении имени
chooseApiPaginatedData :: Int -> String -> (Int -> Int -> String -> IO [a]) -> (a -> String) -> String -> String -> IO a
chooseApiPaginatedData page search getData getName inputMessage infoMessage = do 
    callCommand "cls"
    putStrLn infoMessage
    putStrLn "Команды:\nназад - предыдущая страница,\nвперёд - следующая страница,\nпоиск %name%- поиск по названию,\nсбросить - сбросить фильтры\n"
    consoleInfo inputMessage
    putStrLn $ "\nТекущая страница: " ++ show (page + 1)
    arrayData <- getData (page * 20) (page * 20 + 19) search
    (index, command) <- choosePaginatedData arrayData (\array -> generateLogData array getName) 
    case command of 
        "назад" -> do 
            if page == 0 then chooseApiPaginatedData 0 "" getData getName inputMessage infoMessage
            else chooseApiPaginatedData (page - 1) "" getData getName inputMessage infoMessage
        "сбросить" -> chooseApiPaginatedData 0 "" getData getName inputMessage infoMessage
        "вперёд" -> chooseApiPaginatedData (page + 1) "" getData getName inputMessage infoMessage
        _ -> do 
            if index == -1 then chooseApiPaginatedData 0 command getData getName inputMessage infoMessage
            else return (arrayData !! (index - 1))

-- страница, поисковое имя,  функция запроса, куда передаётся максимальный и минимальный индекс, функция для получении имени
chooseApiParamPaginatedData :: Int -> String -> b -> (Int -> Int -> String -> b -> IO [a]) -> (a -> String) -> String -> String ->  IO a
chooseApiParamPaginatedData page search paramId getData getName inputMessage infoMessage = do 
    callCommand "cls"
    putStrLn infoMessage
    putStrLn "Команды:\nназад - предыдущая страница,\nвперёд - следующая страница,\nпоиск %name%- поиск по названию,\nсбросить - сбросить фильтры\n"
    consoleInfo inputMessage
    putStrLn $ "\nТекущая страница: " ++ show (page + 1)
    arrayData <- getData (page * 20) (page * 20 + 19) search paramId
    (index, command) <- choosePaginatedData arrayData (\array -> generateLogData array getName) 
    case command of 
        "назад" -> do 
            if page == 0 then chooseApiParamPaginatedData 0 "" paramId getData getName inputMessage infoMessage
            else chooseApiParamPaginatedData (page - 1) "" paramId getData getName inputMessage infoMessage
        "сбросить" -> chooseApiParamPaginatedData 0 "" paramId getData getName inputMessage infoMessage
        "вперёд" -> chooseApiParamPaginatedData (page + 1) "" paramId getData getName inputMessage infoMessage
        _ -> do 
            if index == -1 then chooseApiParamPaginatedData 0 command paramId getData getName inputMessage infoMessage
            else return (arrayData !! (index - 1))
          