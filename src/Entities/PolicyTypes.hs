module Entities.PolicyTypes (PolicyType(..), getPolicyTypes, getPolicyTypeById) where

import Shared.Api.GetAllData
import Shared.Api.GetFilterData

data PolicyType = PolicyType { uid :: Int, name :: String } deriving (Read, Show)

getPolicyTypes :: IO [PolicyType]
getPolicyTypes = getAllData "database/PolicyTypes.hdb"

getPolicyTypeById :: Int -> IO PolicyType
getPolicyTypeById policyId = do 
    policies <- getFilterData "database/PolicyTypes.hdb"  0 10000 "" (\_ -> "") (\policy -> uid policy == policyId)
    return $ policies !! (0)