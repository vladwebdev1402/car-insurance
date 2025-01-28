module Entities.TransportCertificate (TransportCertificate(..), getTransportCertificateByNumber, getTransportCertificatesByDriver) where

import Shared.Api.GetFilterData
import Shared.Api.GetAllData

data TransportCertificate = TransportCertificate {uid :: Int, transportId :: Int, driverId :: Int, registrationNumber :: String} deriving (Read, Show)

getTransportCertificateByNumber :: String -> IO (Maybe TransportCertificate)
getTransportCertificateByNumber number = do
    transportCertificates <- getFilterData "database/TransportCertificate.hdb" 0 10000 "" (\_ -> "") (\transportCertificate -> registrationNumber transportCertificate == number)
    case length transportCertificates of
        0 -> return Nothing
        _ -> return $ Just (transportCertificates !! (0))

getTransportCertificatesByDriver :: Int -> IO [TransportCertificate]
getTransportCertificatesByDriver userId = do 
    transportCertificates <- getFilterData "database/TransportCertificate.hdb" 0 10000 "" (\_ -> "") (\transportCertificate -> driverId transportCertificate == userId)
    return transportCertificates

getAllTransportCertificates :: IO [TransportCertificate]
getAllTransportCertificates = getAllData "database/TransportCertificate.hdb"
    