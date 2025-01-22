module Enteties.PolicyTypes (PolicyType(..), getPolicyTypes) where

import System.IO
import GHC.IO.Encoding (setLocaleEncoding)

data PolicyType = PolicyType { uid :: Int, name :: String } deriving (Read, Show)

getPolicyTypes :: Read a => FilePath -> IO [a]
getPolicyTypes filePath = do
    setLocaleEncoding utf8
    file <- openFile filePath ReadMode
    arrayData <- loop file []
    hClose file
    return arrayData
    
    where loop file arrayData = do 
            isEof <- hIsEOF file
            if not isEof
                then do 
                    elementGet <- hGetLine file
                    let element = read elementGet
                    loop file (element : arrayData)
                    
                else return arrayData
