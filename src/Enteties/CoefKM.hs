module Enteties.CoefKM (CoefKM(..), getCoefKM) where

import Shared.Api.GetFilterData

data CoefKM = CoefKM { uid :: Int, companyPolicyLinkId :: Int, typeKMId :: Int, value :: Float } deriving (Read, Show)

getCoefKM :: Int -> Int -> IO CoefKM
getCoefKM linkId kmId = do
    coefs <- getFilterData "database/CoefKM.hdb" 0 10000 "" (\_ -> "") (\coef -> companyPolicyLinkId coef == linkId && typeKMId coef == kmId)
    return $ coefs !! (0)