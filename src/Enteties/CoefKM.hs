module Enteties.CoefKM (CoefKM(..), getCoefKM) where

import Shared.Api.GetFilterData

data CoefKM = CoefKM { uid :: Int, companyPolicyLinkId :: Int, typeKBMId :: Int, value :: Float } deriving (Read, Show)

getCoefKM :: Int -> Int -> IO CoefKM
getCoefKM linkId kbmId = do
    coefs <- getFilterData "database/CoefKM.hdb" 0 10000 "" (\_ -> "") (\coef -> companyPolicyLinkId coef == linkId && typeKBMId coef == kbmId)
    return $ coefs !! (0)