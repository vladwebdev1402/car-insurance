module Shared.Api.InputNewEntity (inputNewEntity) where

import System.IO
import GHC.IO.Encoding (setLocaleEncoding)

inputNewEntity :: Show a => FilePath -> a -> IO ()
inputNewEntity filePath entity = do
    file <- openFile filePath AppendMode
    let entityStr = "\n" ++ show entity
    hSetEncoding file utf8
    hPutStr file entityStr
    hClose file