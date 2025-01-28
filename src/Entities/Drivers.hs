module Entities.Drivers (Driver(..), getDriverByPassport, getDriverById, addNewDriver) where

import Shared.Api.GetFilterData
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

getDriverStr :: Driver -> String
getDriverStr driver = "{" ++ "uid" ++ " = " ++ (show (uid driver)) ++ "," ++
    "surName" ++ " = " ++  (surName driver) ++ "," ++
    "firstName" ++ " = " ++  (firstName driver) ++ "," ++
    "patroName" ++ " = " ++  (firstName driver) ++ "," ++
    "experience" ++ " = " ++ (show (experience driver)) ++ "," ++
    "driverLevel" ++ " = " ++ (show (driverLevel driver)) ++ "," ++
    "numberPassport" ++ " = " ++ (show (numberPassport driver)) ++ "," ++
    "seriePassport" ++ " = " ++ (show (seriePassport driver)) ++ "," ++
    "birthday" ++ " = " ++ (birthday driver) ++ "}"

getDriverByPassport :: Int -> Int -> IO (Maybe Driver)
getDriverByPassport serie number = do
    drivers <- getFilterData "database/Drivers.hdb" 0 10000 "" (\_ -> "") (\driver -> seriePassport driver == serie && numberPassport driver == number)
    case length drivers of
        0 -> return Nothing
        _ -> return $ Just (drivers !! (0))

getDriverById :: Int -> IO (Driver)
getDriverById driverId = do
    drivers <- getFilterData "database/Drivers.hdb" 0 10000 "" (\_ -> "") (\driver -> uid driver == driverId)
    return $ drivers !! (0)

addNewDriver :: Driver -> IO Driver
addNewDriver driver = do
    allDrivers <- getAllData "database/Drivers.hdb" :: IO [Driver]
    let maxUid = if null allDrivers
                    then 0 
                    else foldl (\acc p -> max acc (uid p)) 0 allDrivers 
    let newDriver = driver { uid = maxUid + 1 } 
    inputNewString "database/Drivers.hdb" (getDriverStr newDriver)
    return newDriver
    
   