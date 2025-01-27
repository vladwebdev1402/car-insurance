module Entities.TransportModels (TransportModel(..), getTransportModels, getTransportModelsByBrandId, getTransportModelById) where

import Shared.Api.GetFilterData

data TransportModel = TransportModel {uid :: Int, transportBrandId :: Int, name :: String} deriving (Read, Show)

getTransportModels :: Int -> Int -> String -> IO [TransportModel]
getTransportModels minIdx maxIdx search = getFilterData "database/TransportModels.hdb" minIdx maxIdx search name (\_ -> True)

getTransportModelsByBrandId :: Int -> Int -> String -> Int -> IO [TransportModel]
getTransportModelsByBrandId minIdx maxIdx search brandId = getFilterData "database/TransportModels.hdb" minIdx maxIdx search name (\x -> transportBrandId x == brandId)

getTransportModelById :: Int -> IO TransportModel
getTransportModelById modelId = do 
    transports <- getFilterData "database/TransportModels.hdb" 0 10000 "" (\_ -> "") (\x -> uid x == modelId)
    return $ transports !! (0)