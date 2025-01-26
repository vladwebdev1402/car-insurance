module Views.InformationInsurance (informationInsurance) where

import Text.Printf (printf)
import Data.Maybe (isNothing)
import Data.List (sortBy, all)
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
import Shared.Calc.CalcDaysSince

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
        _ -> showPolicies certificates Nothing

getPoliciesWithCertificates :: [TransportCertificate] -> IO [(TransportCertificate, Policy)]
getPoliciesWithCertificates certificates = do 
    policies <- mapM (\cert -> do 
        policies <- getPoliciesByCertificate (Enteties.TransportCertificate.uid cert)
        return (cert, policies)
        ) certificates

    let filteredPolicies = filter (\(_, policies) -> (length policies) /= 0) policies

    let flattenPolicies = concatMap (\(cert, policies) -> map (\policy -> (cert, policy)) policies) filteredPolicies

    return flattenPolicies


showPolicies :: [TransportCertificate] -> Maybe [FullPolicyInfo] -> IO ()
showPolicies certificates oldFullInfos = do
    callCommand "cls"
    
    fullInfos <- maybe (getPoliciesWithCertificates certificates >>= getFullInfoForPolicy) return oldFullInfos

    choosedPolicy <- choosePolicy fullInfos 0 "" ""

    case choosedPolicy of 
        Nothing -> return ()
        Just policy -> do 
           deactivePolicy fullInfos policy

deactivePolicy :: [FullPolicyInfo] -> FullPolicyInfo -> IO ()
deactivePolicy fullInfos choosedInfo = do
    resultChecked <- checkActivePolicy choosedInfo

    if resultChecked then do
        let link = (Modules.FullPolicyInfo.link choosedInfo)
        
        let policy = (Modules.FullPolicyInfo.policy choosedInfo)

        let (percent, priceRegistration, countDays, date) = ((amountPercent link) / 100, 
                                                        Enteties.Policies.sumInsurance policy, 
                                                        Enteties.Policies.countDays policy,
                                                        Enteties.Policies.date policy)
        daySince <- calcDaysSince date

        let daysPercent = (fromIntegral (countDays - daySince) :: Float) / (fromIntegral countDays :: Float)

        let sumRemain = (priceRegistration - (percent * priceRegistration)) * daysPercent
        
        newPolicy <- updatePolicy policy {status = "deactive", sumRemaininInsurance = sumRemain}

        newFullInfos <- mapM (\item -> do 
            if Enteties.Policies.uid (Modules.FullPolicyInfo.policy item) == Enteties.Policies.uid policy then return item {Modules.FullPolicyInfo.policy = newPolicy}
            else return item
            ) fullInfos

        putStrLn ("\nСтраховка деактивирована\nСумма возврата: " ++ (printf "%.2f" sumRemain))
        putStrLn "Чтобы продолжить, нажмите Enter"
        getLine

        showPolicies (map Modules.FullPolicyInfo.certificate fullInfos) (Just newFullInfos)

    else showPolicies (map Modules.FullPolicyInfo.certificate fullInfos) (Just fullInfos)

checkActivePolicy :: FullPolicyInfo -> IO Bool 
checkActivePolicy choosedInfo = do
    if Enteties.Policies.policyTypeId (Modules.FullPolicyInfo.policy choosedInfo) == 0 then do
        let certId = Enteties.TransportCertificate.uid (Modules.FullPolicyInfo.certificate choosedInfo)

        policies <- mapM (\f -> f) [getActivePolicy certId 1, getActivePolicy certId 2]

        if (all isNothing policies) then return True
        else do
            putStrLn "Вы не можете деактивировать полис ОСАГО, так как у вас имеется активный ДСАГО или КАСКО полис. \nНажмите Enter чтобы продолжить"
            getLine
            return False

    else return True
