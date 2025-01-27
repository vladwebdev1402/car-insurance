module Entities.TypeKS (TypeKS(..), getTypeKS) where

import Shared.Api.GetAllData

data TypeKS = TypeKS { uid :: Int, countMonths :: Float, description :: String, isIsago :: Bool, coefOsago :: Float } deriving (Read, Show)

getTypeKS :: IO [TypeKS]
getTypeKS = getAllData "database/TypesKS.hdb"