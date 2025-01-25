module Enteties.Policies (Policy(..), getActiveOsagoPolicy, addNewPolicy) where

import Shared.Api.GetAllData
import Shared.Api.GetFilterData
import Shared.Api.InputNewEntity

data Policy = Policy { uid :: Int, companyPolicyLinkId :: Int, policyTypeId :: Int, transportCertificateId :: Int, status :: String, countDays :: Int, sumInsurance :: Float, sumRemaininInsurance :: Float, sumDeductible :: Float, date :: String } deriving (Read, Show)

getActiveOsagoPolicy :: Int -> IO (Maybe Policy)
getActiveOsagoPolicy certificateId = do 
    policies <- getFilterData "database/Policies.hdb" 0 10000 "" (\_ -> "") (\policy -> status policy == "active" && transportCertificateId policy == certificateId)
    case length policies of
        0 -> return Nothing
        _ -> do
            let osago = filter (\policy -> policyTypeId policy == 0) policies
            case length osago of
                0 -> return Nothing
                _ -> return $ Just (osago !! (0))

addNewPolicy :: Policy -> IO (Policy)
addNewPolicy policy = do
    allPolicys <- getAllData "database/Policies.hdb" :: IO [Policy]
    let newUid = length allPolicys
    let newPolicy = policy { uid = newUid }
    inputNewEntity "database/Policies.hdb" newPolicy
    return newPolicy