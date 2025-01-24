module Shared.Logs.LogData (generateLogData, generateLogString) where

import System.IO

generateLogData :: [a] -> (a -> String) -> IO ()   
generateLogData arrayData getName = mapM_ (\(index, item) -> putStrLn $ show (index + 1) ++ ". " ++ getName item) (zip [0..] arrayData)
 
generateLogString :: [a] -> (a -> String) -> String
generateLogString arrayData getName = unlines $ map (\(index, item) -> show (index + 1) ++ ". " ++ getName item ) (zip [0..] arrayData)
