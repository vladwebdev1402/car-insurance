module Modules.ChooseTransportBrand (chooseTransportBrand) where

import Data.List.Split    
import System.Process (callCommand)
import Enteties.TransportBrands
import Shared.Inputs.ChooseData (choosePaginatedData)
import Shared.Logs.LogData

chooseTransportBrand :: Int -> String -> IO (TransportBrand)
chooseTransportBrand page search = do 
    callCommand "cls"
    transportBrands <- getTransportBrands (page * 10) (page * 10 + 10) search
    (index, command) <- choosePaginatedData transportBrands (\array -> generateLogData array name) 
    case command of 
        "назад" -> chooseTransportBrand (page - 1) ""
        "вперёд" -> chooseTransportBrand (page + 1) ""
        _ -> case index of
            -1 -> chooseTransportBrand 0 command
            _ -> return (transportBrands !! (index - 1))