
module Entities.CoefAdditional (CoefAdditional(..), getCoefAdditional) where

import Shared.Api.GetFilterData

data CoefAdditional = CoefAdditional { uid :: Int, companyPolicyLinkId :: Int, additionalId :: Int, value :: Float } deriving (Read, Show)

getCoefAdditional :: Int -> Int -> IO (Maybe CoefAdditional)
getCoefAdditional linkId addId = do
    coefs <- getFilterData "database/CoefAdditional.hdb" 0 10000 "" (\_ -> "") (\coef -> companyPolicyLinkId coef == linkId && additionalId coef == addId)
    case (length coefs) of 
        0 -> return Nothing
        _ -> return $ Just (coefs !! (0))