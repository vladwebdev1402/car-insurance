module Enteties.Policies (Policy(..), getActivePolicy, addNewPolicy, getPoliciesByCertificate, policyStatusRuTranslate, policyStatusEngTranslate) where

import Shared.Api.GetAllData
import Shared.Api.GetFilterData
import Shared.Api.InputNewEntity

data Policy = Policy { uid :: Int, 
                    companyPolicyLinkId :: Int, 
                    policyTypeId :: Int, 
                    transportCertificateId :: Int,
                    status :: String, 
                    countDays :: Int, 
                    sumInsurance :: Float, 
                    sumRemaininInsurance :: Float, 
                    sumDeductible :: Float, 
                    date :: String } deriving (Read, Show)

getActivePolicy :: Int -> Int -> IO (Maybe Policy)
getActivePolicy certificateId polyceType = do 
    policies <- getFilterData "database/Policies.hdb" 0 10000 "" (\_ -> "") (\policy -> status policy == "active" && transportCertificateId policy == certificateId && policyTypeId policy == polyceType)
    case length policies of
        0 -> return Nothing
        _ -> return $ Just (policies !! (0))

getPoliciesByCertificate :: Int -> IO [Policy]
getPoliciesByCertificate certificateId = getFilterData "database/Policies.hdb" 0 10000 "" (\_ -> "") (\policy -> transportCertificateId policy == certificateId)


addNewPolicy :: Policy -> IO (Policy)
addNewPolicy policy = do
    allPolicys <- getAllData "database/Policies.hdb" :: IO [Policy]
    let maxUid = if null allPolicys
                    then 0 
                    else foldl (\acc p -> max acc (uid p)) 0 allPolicys 
    let newPolicy = policy { uid = maxUid + 1 } 
    inputNewEntity "database/Policies.hdb" newPolicy
    return newPolicy

policyStatusRuTranslate :: String -> String 
policyStatusRuTranslate "active" = "активен"
policyStatusRuTranslate "deactive" = "не активен"

policyStatusEngTranslate :: String -> String 
policyStatusEngTranslate "активен" = "active"
policyStatusEngTranslate "не активен" = "deactive"
