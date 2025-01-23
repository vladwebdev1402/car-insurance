module Enteties.TransportBrands (TransportBrand(..), getTransportBrands) where

import Shared.Api.GetFilterData

data TransportBrand = TransportBrand {uid :: Int, name :: String} deriving (Read, Show)

getTransportBrands :: Int -> Int -> String -> IO [TransportBrand]
getTransportBrands minIdx maxIdx search = getFilterData "database/TransportBrands.hdb" minIdx maxIdx search name (\_ -> True)