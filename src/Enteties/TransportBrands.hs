module Enteties.TransportBrands (TransportBrand(..), getTransportBrands) where

import System.IO
import GHC.IO.Encoding (setLocaleEncoding)
import Data.List

data TransportBrand = TransportBrand {uid :: Int, name :: String} deriving (Read, Show)

getTransportBrands :: Int -> Int -> String -> IO [TransportBrand]
getTransportBrands minIdx maxIdx search = do
    setLocaleEncoding utf8
    file <- openFile "database/TransportBrands.hdb" ReadMode
    arrayData <- loop file []
    hClose file
    return (take (maxIdx - minIdx + 1) (drop minIdx arrayData))

    where loop file arrayData = do 
            isEof <- hIsEOF file
            if not isEof
                then do 
                    elementGet <- hGetLine file
                    let element = read elementGet :: TransportBrand
                    if search `isInfixOf` (name element) then loop file (arrayData ++ [element])
                    else loop file arrayData
                else return arrayData

