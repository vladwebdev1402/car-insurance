module Enteties.PolicyCase (PolicyCase(..), getPolicyCases, getPolicyCasesInfo) where

import Shared.Api.GetFilterData

data PolicyCase = PolicyCase {uid :: Int, policyId :: Int, sumDamage :: Float} deriving (Show, Read)

getPolicyCases :: Int -> IO [PolicyCase]
getPolicyCases policyUid = getFilterData "database/PolicyCase.hdb" 0 10000 "" (\_ -> "") (\polCase -> policyId polCase == policyUid)

getPolicyCasesInfo :: [PolicyCase] -> String
getPolicyCasesInfo cases =
    (case (length cases) of 
        0 -> ""
        count -> "\nКоличество страховых случаев: " ++ (show count) ++ "\nСуммарный ущерб: " ++ (show (sum $ map (\item -> (sumDamage item)) cases))
    )