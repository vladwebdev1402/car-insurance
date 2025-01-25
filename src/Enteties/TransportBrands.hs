module Enteties.TransportBrands (TransportBrand(..), getTransportBrands, getTransportBrandById) where

import Shared.Api.GetFilterData

data TransportBrand = TransportBrand {uid :: Int, name :: String} deriving (Read, Show)

getTransportBrands :: Int -> Int -> String -> IO [TransportBrand]
getTransportBrands minIdx maxIdx search = getFilterData "database/TransportBrands.hdb" minIdx maxIdx search name (\_ -> True)

getTransportBrandById :: Int -> IO TransportBrand
getTransportBrandById brandId = do 
    brands <- getFilterData "database/TransportBrands.hdb" 0 10000 "" (\x -> "") (\x -> uid x == brandId)
    return $ brands !! (0)