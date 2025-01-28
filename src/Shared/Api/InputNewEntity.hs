module Shared.Api.InputNewEntity (inputNewEntity, inputNewString) where

import Main.Utf8
import System.IO
import GHC.IO.Encoding (setLocaleEncoding)

inputNewEntity :: Show a => FilePath -> a -> IO ()
inputNewEntity filePath entity = withUtf8 $ do
    file <- openFile filePath AppendMode
    let entityStr = "\n" ++ show entity
    hPutStr file entityStr
    hClose file

inputNewString :: FilePath -> String -> IO ()
inputNewString filePath str = withUtf8 $ do
    file <- openFile filePath AppendMode
    hPutStr file ("\n" ++ str)
    hClose file