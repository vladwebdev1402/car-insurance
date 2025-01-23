module Enteties.Regions (Region(..), getRegions) where

import Shared.Api.GetFilterData

data Region = Region { uid :: Int, name :: String } deriving (Read, Show)

getRegions :: Int -> Int -> String -> IO [Region]
getRegions minIdx maxIdx search = getFilterData "database/Regions.hdb" minIdx maxIdx search name (\_ -> True)