module Entities.TypesTransport (TypeTransport(..), getTypesTransport, getTypeTransportById, getNullTypeTransport) where

import Shared.Api.GetAllData
import Shared.Api.GetFilterData

data TypeTransport = TypeTransport { uid :: Int, description :: String, minPriceCoefOsago :: Float, maxPriceCoefOsago :: Float } deriving (Read, Show)

getTypesTransport :: IO [TypeTransport]
getTypesTransport = getAllData "database/TypesTransport.hdb"

getTypeTransportById :: Int -> IO TypeTransport
getTypeTransportById searchId = do
    types <- getFilterData "database/TypesTransport.hdb" 0 100 "" (\x -> description x) (\x -> uid x == searchId)
    return $ types !! (0)

getNullTypeTransport :: TypeTransport
getNullTypeTransport = TypeTransport { uid = 0, description = "", minPriceCoefOsago = 0.0, maxPriceCoefOsago = 0.0 }