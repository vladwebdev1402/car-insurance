module Shared.Api.GetFilterData (getFilterData) where

import System.IO
import GHC.IO.Encoding (setLocaleEncoding)
import Data.List
import Data.Char (toLower)

getFilterData :: Read a => FilePath -> Int -> Int -> String -> (a -> String) -> (a -> Bool) -> IO [a]
getFilterData filePath minIdx maxIdx search getName filter = do
    setLocaleEncoding utf8
    file <- openFile filePath ReadMode
    arrayData <- loop file []
    hClose file
    return (take (maxIdx - minIdx + 1) (drop minIdx arrayData))

    where loop file arrayData = do 
            isEof <- hIsEOF file
            if not isEof
                then do 
                    elementGet <- hGetLine file
                    let element = read elementGet
                    if (map toLower search) `isInfixOf` (map toLower (getName element)) && filter element then loop file (arrayData ++ [element])
                    else loop file arrayData
                else return arrayData