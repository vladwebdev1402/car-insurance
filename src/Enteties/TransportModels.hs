module Enteties.TransportModels (TransportModel(..), getTransportModels, getTransportModelsByBrandId) where

import Shared.Api.GetFilterData

data TransportModel = TransportModel {uid :: Int, transportBrandId :: Int, name :: String} deriving (Read, Show)

getTransportModels :: Int -> Int -> String -> IO [TransportModel]
getTransportModels minIdx maxIdx search = getFilterData "database/TransportModels.hdb" minIdx maxIdx search name (\_ -> True)

getTransportModelsByBrandId :: Int -> Int -> String -> Int -> IO [TransportModel]
getTransportModelsByBrandId minIdx maxIdx search brandId = getFilterData "database/TransportModels.hdb" minIdx maxIdx search name (\x -> transportBrandId x == brandId)

