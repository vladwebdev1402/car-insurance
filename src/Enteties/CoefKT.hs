module Enteties.CoefKT (CoefKT(..), getCoefKT) where

import Shared.Api.GetFilterData

data CoefKT = CoefKT { uid :: Int, companyPolicyLinkId :: Int, territorieId :: Int, value :: Float } deriving (Read, Show)

getCoefKT :: Int -> Int -> IO CoefKT
getCoefKT linkId terId = do
    coefs <- getFilterData "database/CoefKT.hdb" 0 10000 "" (\_ -> "") (\coef -> companyPolicyLinkId coef == linkId && territorieId coef == terId)
    return $ coefs !! (0)