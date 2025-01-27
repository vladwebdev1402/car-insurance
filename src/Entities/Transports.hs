module Entities.Transports (Transport(..), getTransports, getTransportByModelId, getTransportById) where

import Shared.Api.GetFilterData

data Transport = Transport {uid :: Int, transportModelId :: Int, typeTransportId :: Int, year :: Int, power :: Int} deriving (Read, Show)

getTransports :: Int -> Int -> String -> IO [Transport]
getTransports minIdx maxIdx search = getFilterData "database/Transports.hdb" minIdx maxIdx search (\x -> (show (year x))) (\_ -> True)

getTransportByModelId :: Int -> Int -> String -> Int -> IO [Transport]
getTransportByModelId minIdx maxIdx search modelId = getFilterData "database/Transports.hdb" minIdx maxIdx search (\x -> (show (year x))) (\x -> transportModelId x == modelId)

getTransportById :: Int -> IO Transport
getTransportById transportId = do 
    transports <- getFilterData "database/Transports.hdb" 0 10000 "" (\x -> "") (\x -> uid x == transportId)
    return $ transports !! (0)