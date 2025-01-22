module Shared.Inputs.ChooseData (chooseData, choosePaginatedData) where

import System.Console.ANSI
import System.Process (callCommand)
import Shared.Validators.ValidateNumberRangeInput
import Data.List.Split 
import Data.List

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