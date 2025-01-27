module Entities.TypesKM (TypeKM(..), getTypeKMByPower) where

import Shared.Api.GetFilterData

data TypeKM = TypeKM { uid :: Int, minEnginePower :: Int, maxEnginePower :: Int, isOsago :: Bool, coefOsago :: Float } deriving (Read, Show)

getTypeKMByPower :: Int -> Bool -> IO TypeKM
getTypeKMByPower power True = do 
    typesKm <- getFilterData "database/TypesKM.hdb" 0 10000 "" (\_ -> "") (\km -> minEnginePower km <= power && maxEnginePower km >= power && isOsago km == True)
    return $ typesKm !! (0)

getTypeKMByPower power False = do 
    typesKm <- getFilterData "database/TypesKM.hdb" 0 10000 "" (\_ -> "") (\km -> minEnginePower km <= power && maxEnginePower km >= power)
    return $ typesKm !! (0)