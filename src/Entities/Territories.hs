module Entities.Territories (Territorie(..), getTerritories, getTerritoriesByRegionId) where

import Shared.Api.GetFilterData

data Territorie = Territorie {uid :: Int, regionId :: Int, name :: String, coefOsago :: Float} deriving (Read, Show)

getTerritories :: Int -> Int -> String -> IO [Territorie]
getTerritories minIdx maxIdx search = getFilterData "database/Territories.hdb" minIdx maxIdx search name (\_ -> True)

getTerritoriesByRegionId :: Int -> Int -> String -> Int -> IO [Territorie]
getTerritoriesByRegionId minIdx maxIdx search region = getFilterData "database/Territories.hdb" minIdx maxIdx search name (\x -> regionId x == region)
