module Enteties.Transports (Transport(..), getTransports, getTransportByModelId) where

import System.IO
import Shared.Api.GetFilterData

data Transport = Transport {uid :: Int, transportModelId :: Int, typeTransportId :: Int, year :: Int, power :: Int} deriving (Read, Show)

getTransports :: Int -> Int -> String -> IO [Transport]
getTransports minIdx maxIdx search = getFilterData "database/Transports.hdb" minIdx maxIdx search (\x -> (show (year x))) (\x -> True)

getTransportByModelId :: Int -> Int -> String -> Int -> IO [Transport]
getTransportByModelId minIdx maxIdx search modelId = getFilterData "database/Transports.hdb" minIdx maxIdx search (\x -> (show (year x))) (\x -> transportModelId x == modelId)

