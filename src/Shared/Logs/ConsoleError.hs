module Shared.Logs.ConsoleError (consoleError) where 

import System.Console.ANSI

consoleError :: String -> IO ()
consoleError message = do
    setSGR [SetColor Foreground Vivid Red]
    putStrLn message
    setSGR [Reset]
