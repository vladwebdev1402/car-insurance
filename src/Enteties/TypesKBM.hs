module Enteties.TypesKBM (TypeKBM(..), getTypeKBMByDriverLever) where

import Shared.Api.GetFilterData

data TypeKBM = TypeKBM { uid :: Int, driverLevel :: Int, coefOsago :: Float } deriving (Read, Show)

getTypeKBMByDriverLever :: Int -> IO TypeKBM
getTypeKBMByDriverLever level = do 
    typesKbm <- getFilterData "database/TypesKBM.hdb" 0 10000 "" (\_ -> "") (\kbm -> driverLevel kbm == level)
    return $ typesKbm !! (0)