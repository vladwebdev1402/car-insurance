module Enteties.TypesKVS (TypeKVS(..), getTypeKVS) where

import Shared.Api.GetFilterData

data TypeKVS = TypeKVS { uid :: Int, 
                        minAge :: Int, 
                        maxAge :: Int, 
                        minDrivingExpirience :: Int, 
                        maxDrivingExpirience :: Int,
                        isOsago :: Bool,
                        coefOsago :: Float } deriving (Read, Show)

getTypeKVS :: Int -> Int -> Bool -> IO TypeKVS
getTypeKVS age drivingExpirience True = do 
            coefs <- getFilterData "database/TypesKVS.hdb" 0 10000 "" (\_ -> "") (\kvs -> minAge kvs <= age && maxAge kvs >= age && minDrivingExpirience kvs <= drivingExpirience && maxDrivingExpirience kvs >= drivingExpirience && isOsago kvs == True)
            return $ coefs !! (0)


getTypeKVS age drivingExpirience False = do 
            coefs <- getFilterData "database/TypesKVS.hdb" 0 10000 "" (\_ -> "") (\kvs -> minAge kvs <= age && maxAge kvs >= age && minDrivingExpirience kvs <= drivingExpirience && maxDrivingExpirience kvs >= drivingExpirience)
            return $ coefs !! (0)