module Enteties.CoefDeductible (CoefDeductible(..), getCoefDeductible) where

import Shared.Api.GetFilterData

data CoefDeductible = CoefDeductible { uid :: Int, companyPolicyLinkId :: Int, deductibleId :: Int, value :: Float } deriving (Read, Show)

getCoefDeductible :: Int -> Int -> IO CoefDeductible
getCoefDeductible linkId deducId = do
    coefs <- getFilterData "database/CoefDeductible.hdb" 0 10000 "" (\_ -> "") (\coef -> companyPolicyLinkId coef == linkId && deductibleId coef == deducId)
    return $ coefs !! (0)