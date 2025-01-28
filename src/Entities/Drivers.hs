module Entities.Drivers (Driver(..), getDriverByPassport, getDriverById, addNewDriver) where

import Shared.Api.GetFilterData
import Main.Utf8
import Data.List (intercalate)
import Shared.Api.GetAllData
import Shared.Api.InputNewEntity

data Driver = Driver { uid :: Int, 
    surName :: String, 
    firstName :: String, 
    patroName :: String, 
    experience :: Int, 
    driverLevel :: Int, 
    numberPassport :: Int, 
    seriePassport :: Int, 
    birthday :: String } deriving (Read, Show)

getDriverByPassport :: Int -> Int -> IO (Maybe Driver)
getDriverByPassport serie number = do
    drivers <- getFilterData "database/Drivers.txt" 0 10000 "" (\_ -> "") (\driver -> seriePassport driver == serie && numberPassport driver == number)
    case length drivers of
        0 -> return Nothing
        _ -> return $ Just (drivers !! (0))

getDriverById :: Int -> IO (Driver)
getDriverById driverId = do
    drivers <- getFilterData "database/Drivers.txt" 0 10000 "" (\_ -> "") (\driver -> uid driver == driverId)
    return $ drivers !! (0)

addNewDriver :: Driver -> IO Driver
addNewDriver driver = withUtf8 $ do
    allDrivers <- getAllData "database/Drivers.txt" :: IO [Driver]
    let maxUid = if null allDrivers
                    then 0 
                    else foldl (\acc p -> max acc (uid p)) 0 allDrivers 
    let newDriver = driver { uid = maxUid + 1 } 
    inputNewEntity "database/Drivers.txt" newDriver
    return newDriver
    
   