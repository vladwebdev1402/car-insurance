module Views.InformationInsurance (informationInsurance) where

import Data.List (sortBy)
import System.Process (callCommand)
import Views.Helpers.ChoosePolicy
import Enteties.Drivers
import Enteties.TransportCertificate
import Enteties.Policies
import Enteties.CompanyPolicyLink
import Enteties.Companys
import Enteties.PolicyTypes
import Enteties.PolicyCase
import Modules.FullPolicyInfo
import Shared.Inputs.ChooseData (chooseData)
import Shared.Logs.LogData
import Shared.Inputs.InputPassport

informationInsurance :: String -> IO ()
informationInsurance infoMessage = do 
    callCommand "cls" 
    (serie, number) <- inputPassport (infoMessage ++ "\nЧтобы выйти, введите 'выход'")

    case (serie) of 
        -1 -> return ()
        _ -> do 
            driver <- getDriverByPassport serie number
            checkDriver driver 

checkDriver :: Maybe Driver -> IO ()
checkDriver driver = case driver of 
    Nothing -> informationInsurance "Водитель не был найден по введённым данным"
    Just driver -> getCertificates driver
    
getCertificates :: Driver -> IO ()
getCertificates driver = do 
    transportCertificates <- getTransportCertificatesByDriver (Enteties.Drivers.uid driver) 

    case (length transportCertificates) of
        0 -> informationInsurance "У данного водителя нет зарегестрированных транспортных стредств"
        _ -> getPolicies transportCertificates

getPolicies :: [TransportCertificate] -> IO ()
getPolicies certificates = do
    policiesBooleans <- mapM (\cert -> do 
        policies <- getPoliciesByCertificate (Enteties.TransportCertificate.uid cert)
        case (length policies) of 
            0 -> return False
            _ -> return True
        ) certificates

    case (all (\item -> item == False) policiesBooleans) of
        True -> informationInsurance "У данного водителя не имеется полисов"
        _ -> showPolicies certificates

getPoliciesWithCertificates :: [TransportCertificate] -> IO [(TransportCertificate, Policy)]
getPoliciesWithCertificates certificates = do 
    policies <- mapM (\cert -> do 
        policies <- getPoliciesByCertificate (Enteties.TransportCertificate.uid cert)
        return (cert, policies)
        ) certificates

    let filteredPolicies = filter (\(_, policies) -> (length policies) /= 0) policies

    let flattenPolicies = concatMap (\(cert, policies) -> map (\policy -> (cert, policy)) policies) filteredPolicies

    return flattenPolicies


showPolicies :: [TransportCertificate] -> IO ()
showPolicies certificates = do
    callCommand "cls"
    
    certificatesWithPolicies <- getPoliciesWithCertificates certificates
    
    fullInfos <- getFullInfoForPolicy certificatesWithPolicies

    choosedPolicy <- choosePolicy fullInfos 0 "" ""

    case choosedPolicy of 
        Nothing -> return ()
        policy -> do 
            print policy
            getLine
            putStrLn ""
