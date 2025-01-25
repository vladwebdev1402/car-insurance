module Enteties.Policies (Policy(..), getActivePolicy, addNewPolicy) where

import Shared.Api.GetAllData
import Shared.Api.GetFilterData
import Shared.Api.InputNewEntity

data Policy = Policy { uid :: Int, companyPolicyLinkId :: Int, policyTypeId :: Int, transportCertificateId :: Int, status :: String, countDays :: Int, sumInsurance :: Float, sumRemaininInsurance :: Float, sumDeductible :: Float, date :: String } deriving (Read, Show)

getActivePolicy :: Int -> Int -> IO (Maybe Policy)
getActivePolicy certificateId polyceType = do 
    policies <- getFilterData "database/Policies.hdb" 0 10000 "" (\_ -> "") (\policy -> status policy == "active" && transportCertificateId policy == certificateId && policyTypeId policy == polyceType)
    case length policies of
        0 -> return Nothing
        _ -> return $ Just (policies !! (0))
           

addNewPolicy :: Policy -> IO (Policy)
addNewPolicy policy = do
    allPolicys <- getAllData "database/Policies.hdb" :: IO [Policy]
    let newUid = length allPolicys
    let newPolicy = policy { uid = newUid }
    inputNewEntity "database/Policies.hdb" newPolicy
    return newPolicy