module Enteties.CoefKO (CoefKO(..), getCoefKO) where

import Shared.Api.GetFilterData

data CoefKO = CoefKO { uid :: Int, companyPolicyLinkId :: Int, typeKOId :: Int, value :: Float } deriving (Read, Show)

getCoefKO :: Int -> Int -> IO CoefKO
getCoefKO linkId koId = do
    coefs <- getFilterData "database/CoefKO.hdb" 0 10000 "" (\_ -> "") (\coef -> companyPolicyLinkId coef == linkId && typeKOId coef == koId)
    return $ coefs !! (0)