module Shared.Logs.LogData (generateLogData) where

import System.IO

generateLogData :: [a] -> (a -> String) -> IO ()   
generateLogData arrayData getName = mapM_ (\(index, item) -> putStrLn $ show (index + 1) ++ ". " ++ getName item) (zip [0..] arrayData)
 