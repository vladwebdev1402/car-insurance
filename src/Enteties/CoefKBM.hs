module Enteties.CoefKBM (CoefKBM(..), getCoefKBM) where

import Shared.Api.GetFilterData

data CoefKBM = CoefKBM { uid :: Int, companyPolicyLinkId :: Int, typeKMId :: Int, value :: Float } deriving (Read, Show)

getCoefKBM :: Int -> Int -> IO CoefKBM
getCoefKBM linkId kmId = do
    coefs <- getFilterData "database/CoefKBM.hdb" 0 10000 "" (\_ -> "") (\coef -> companyPolicyLinkId coef == linkId && typeKMId coef == kmId)
    return $ coefs !! (0)