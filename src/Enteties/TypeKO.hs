module Enteties.TypeKO (TypeKO(..), getTypeKO) where

import Shared.Api.GetAllData

data TypeKO = TypeKO { uid :: Int, isLimited :: Bool, description :: String, coefOsago :: Float } deriving (Read, Show)

getTypeKO :: IO [TypeKO]
getTypeKO = getAllData "database/TypesKO.hdb"