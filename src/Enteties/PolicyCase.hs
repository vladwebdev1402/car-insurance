module Enteties.PolicyCase (PolicyCase(..), getPolicyCases) where

import Shared.Api.GetFilterData

data PolicyCase = PolicyCase {uid :: Int, policyId :: Int, sumDamage :: Float} deriving (Show, Read)

getPolicyCases :: Int -> IO [PolicyCase]
getPolicyCases policyUid = getFilterData "database/PolicyCase.hdb" 0 10000 "" (\_ -> "") (\polCase -> policyId polCase == policyUid)