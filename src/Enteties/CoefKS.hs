module Enteties.CoefKS (CoefKS(..), getCoefKS) where

import Shared.Api.GetFilterData

data CoefKS = CoefKS { uid :: Int, companyPolicyLinkId :: Int, typeKSId :: Int, value :: Float } deriving (Read, Show)

getCoefKS :: Int -> Int -> IO CoefKS
getCoefKS linkId ksId = do
    coefs <- getFilterData "database/CoefKS.hdb" 0 10000 "" (\_ -> "") (\coef -> companyPolicyLinkId coef == linkId && typeKSId coef == ksId)
    return $ coefs !! (0)