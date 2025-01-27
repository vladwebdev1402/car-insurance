module Entities.CoefDeductible (CoefDeductible(..), getCoefDeductible) where

import Shared.Api.GetFilterData

data CoefDeductible = CoefDeductible { uid :: Int, companyPolicyLinkId :: Int, deductibleId :: Int, value :: Float } deriving (Read, Show)

getCoefDeductible :: Int -> Int -> IO (Maybe CoefDeductible)
getCoefDeductible linkId deducId = do
    coefs <- getFilterData "database/CoefDeductible.hdb" 0 10000 "" (\_ -> "") (\coef -> companyPolicyLinkId coef == linkId && deductibleId coef == deducId)
    case (length coefs) of 
        0 -> return Nothing
        _ -> return $ Just (coefs !! (0))