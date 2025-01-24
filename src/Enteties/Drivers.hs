module Enteties.Drivers (Driver(..), getDriverByPassport) where

import Shared.Api.GetFilterData

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
    drivers <- getFilterData "database/Drivers.hdb" 0 10000 "" (\_ -> "") (\driver -> seriePassport driver == serie && numberPassport driver == number)
    case length drivers of
        0 -> return Nothing
        _ -> return $ Just (drivers !! (0))
    
   