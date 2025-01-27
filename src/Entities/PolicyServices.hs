module Entities.PolicyServices (PolicyService(..), getPolicyServices) where

import Shared.Api.GetAllData

data PolicyService = PolicyService {uid :: Int, name :: String} deriving (Read, Show)

getPolicyServices :: IO [PolicyService]
getPolicyServices = getAllData "database/PolicyServices.hdb"
