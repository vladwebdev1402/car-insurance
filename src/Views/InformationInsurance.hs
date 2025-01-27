module Views.InformationInsurance (informationInsurance) where

import Text.Printf (printf)
import Data.Maybe (isNothing)
import Data.List (sortBy, all)
import System.Process (callCommand)
import Views.Helpers.ChoosePolicy
import Entities.Drivers
import Entities.TransportCertificate
import Entities.Policies
import Entities.CompanyPolicyLink
import Entities.Companys
import Entities.PolicyTypes
import Entities.PolicyCase
import Modules.FullPolicyInfo
import Shared.Inputs.ChooseData (chooseData)
import Shared.Logs.LogData
import Shared.Logs.FormateNumber
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
    transportCertificates <- getTransportCertificatesByDriver (Entities.Drivers.uid driver) 

    case (length transportCertificates) of
        0 -> informationInsurance "У данного водителя нет зарегестрированных транспортных стредств"
        _ -> getPolicies transportCertificates

getPolicies :: [TransportCertificate] -> IO ()
getPolicies certificates = do
    policiesBooleans <- mapM (\cert -> do 
        policies <- getPoliciesByCertificate (Entities.TransportCertificate.uid cert)
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
        policies <- getPoliciesByCertificate (Entities.TransportCertificate.uid cert)
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
                                                        Entities.Policies.sumInsurance policy, 
                                                        Entities.Policies.countDays policy,
                                                        Entities.Policies.date policy)
        daySince <- calcDaysSince date

        let daysPercent = (fromIntegral (countDays - daySince) :: Float) / (fromIntegral countDays :: Float)

        let checkedCasesCoef = (if length (Modules.FullPolicyInfo.policyCases choosedInfo) /= 0 then 0 else 1)

        let sumRemain = (priceRegistration - (percent * priceRegistration)) * daysPercent * checkedCasesCoef
        
        newPolicy <- updatePolicy policy {status = "deactive", sumRemaininInsurance = sumRemain}

        newFullInfos <- mapM (\item -> do 
            if Entities.Policies.uid (Modules.FullPolicyInfo.policy item) == Entities.Policies.uid policy then return item {Modules.FullPolicyInfo.policy = newPolicy}
            else return item
            ) fullInfos

        putStrLn ("\nСтраховой договор расторгнут\nСумма возврата: " ++ (formateFloat sumRemain))
        putStrLn "Чтобы продолжить, нажмите Enter"
        getLine

        showPolicies (map Modules.FullPolicyInfo.certificate fullInfos) (Just newFullInfos)

    else showPolicies (map Modules.FullPolicyInfo.certificate fullInfos) (Just fullInfos)

checkActivePolicy :: FullPolicyInfo -> IO Bool 
checkActivePolicy choosedInfo = do
    if Entities.Policies.policyTypeId (Modules.FullPolicyInfo.policy choosedInfo) == 0 then do
        let certId = Entities.TransportCertificate.uid (Modules.FullPolicyInfo.certificate choosedInfo)

        policies <- mapM (\f -> f) [getActivePolicy certId 1, getActivePolicy certId 2]

        if (all isNothing policies) then return True
        else do
            putStrLn "Вы не можете деактивировать полис ОСАГО, так как у вас имеется активный ДСАГО или КАСКО полис. \nНажмите Enter чтобы продолжить"
            getLine
            return False

    else return True
