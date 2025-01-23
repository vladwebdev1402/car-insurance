module Shared.Logs.Console (consoleError, consoleInfo) where 

import System.Console.ANSI

consoleError :: String -> IO ()
consoleError message = do
    setSGR [SetColor Foreground Vivid Red]
    putStrLn message
    setSGR [Reset]

consoleInfo :: String -> IO ()
consoleInfo message = do
    setSGR [SetColor Foreground Dull Green] 
    putStrLn message
    setSGR [Reset]  
