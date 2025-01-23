module Shared.Inputs.ChooseData (chooseData, choosePaginatedData, chooseApiPaginatedData, chooseApiParamPaginatedData) where

import System.Console.ANSI
import System.Process (callCommand)
import Data.List.Split 
import Data.List
import Shared.Validators.ValidateNumberRangeInput
import Shared.Logs.LogData

chooseData :: [a] -> ([a] -> IO ()) -> IO Int
chooseData arrayData displayFunction = do
    displayFunction arrayData
    input <- getLine
    case validateNumberRangeInput 1 (length arrayData) input of
        Just num -> return num
        Nothing -> do
            callCommand "cls" 
            setSGR [SetColor Foreground Vivid Red]
            putStrLn $ "Ошибка: введите число от 1 до " ++ show (length arrayData) ++ "."
            setSGR [Reset]
            chooseData arrayData displayFunction

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
                            setSGR [SetColor Foreground Vivid Red]
                            putStrLn $ "Введите название для поиска"
                            setSGR [Reset]
                            choosePaginatedData arrayData displayFunction
                        else do
                            let [_, name] = parts
                            return (-1, name)
                    else do
                        callCommand "cls" 
                        setSGR [SetColor Foreground Vivid Red]
                        putStrLn $ "Ошибка: введите число от 1 до " ++ show (length arrayData) ++ "."
                        setSGR [Reset]
                        choosePaginatedData arrayData displayFunction

-- страница, поисковое имя, функция запроса, куда передаётся максимальный и минимальный индекс, функция для получении имени
chooseApiPaginatedData :: Int -> String -> (Int -> Int -> String -> IO [a]) -> (a -> String) -> IO a
chooseApiPaginatedData page search getData getName = do 
    callCommand "cls"
    putStrLn "Команды:\nназад - предыдущая страница,\nвперёд - следующая страница,\nпоиск %name%- поиск по названию,\nсбросить - сбросить фильтры"
    putStrLn $ "\nТекущая страница: " ++ show (page + 1)
    arrayData <- getData (page * 10) (page * 10 + 9) search
    (index, command) <- choosePaginatedData arrayData (\array -> generateLogData array getName) 
    case command of 
        "назад" -> do 
            if page == 0 then chooseApiPaginatedData 0 "" getData getName 
            else chooseApiPaginatedData (page - 1) "" getData getName
        "сбросить" -> chooseApiPaginatedData 0 "" getData getName
        "вперёд" -> chooseApiPaginatedData (page + 1) "" getData getName
        _ -> do 
            if index == -1 then chooseApiPaginatedData 0 command getData getName
            else return (arrayData !! (index - 1))

-- страница, поисковое имя, функция запроса, куда передаётся максимальный и минимальный индекс, функция для получении имени
chooseApiParamPaginatedData :: Int -> String -> Int -> (Int -> Int -> String -> Int -> IO [a]) -> (a -> String) -> IO a
chooseApiParamPaginatedData page search paramId getData getName = do 
    callCommand "cls"
    putStrLn "Команды:\nназад - предыдущая страница,\nвперёд - следующая страница,\nпоиск %name%- поиск по названию,\nсбросить - сбросить фильтры"
    putStrLn $ "\nТекущая страница: " ++ show (page + 1)
    arrayData <- getData (page * 10) (page * 10 + 9) search paramId
    (index, command) <- choosePaginatedData arrayData (\array -> generateLogData array getName) 
    case command of 
        "назад" -> do 
            if page == 0 then chooseApiParamPaginatedData 0 "" paramId getData getName 
            else chooseApiParamPaginatedData (page - 1) "" paramId getData getName
        "сбросить" -> chooseApiParamPaginatedData 0 "" paramId getData getName
        "вперёд" -> chooseApiParamPaginatedData (page + 1) "" paramId getData getName
        _ -> do 
            if index == -1 then chooseApiParamPaginatedData 0 command paramId getData getName
            else return (arrayData !! (index - 1))
          