module Entities.TransportCertificate (TransportCertificate(..), getTransportCertificateByNumber, getTransportCertificatesByDriver, addNewTransportCertificates) where

import Shared.Api.GetFilterData
import Shared.Api.GetAllData
import Shared.Api.InputNewEntity

data TransportCertificate = TransportCertificate {uid :: Int, transportId :: Int, driverId :: Int, registrationNumber :: String} deriving (Read, Show)

getTransportCertificateStr :: TransportCertificate -> String
getTransportCertificateStr cert = "TransportCertificate {" ++ "uid" ++ " = " ++ (show (uid cert)) ++ "," ++
    "transportId" ++ " = " ++  (show (transportId cert)) ++ "," ++
    "driverId" ++ " = " ++  (show (driverId cert)) ++ "," ++
    "registrationNumber" ++ " = " ++  "\"" ++ (registrationNumber cert) ++ "\"" ++  "}"

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

addNewTransportCertificates :: TransportCertificate -> IO TransportCertificate
addNewTransportCertificates cert = do
    certificates <- getAllData "database/TransportCertificate.hdb" :: IO [TransportCertificate]
    let maxUid = if null certificates
                    then 0 
                    else foldl (\acc p -> max acc (uid p)) 0 certificates 
    let newCert = cert { uid = maxUid + 1 } 
    inputNewString "database/TransportCertificate.hdb" (getTransportCertificateStr newCert)
    return newCert
    