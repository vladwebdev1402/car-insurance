module Entities.CoefKO (CoefKO(..), getCoefKO) where

import Shared.Api.GetFilterData

data CoefKO = CoefKO { uid :: Int, companyPolicyLinkId :: Int, typeKOId :: Int, value :: Float } deriving (Read, Show)

getCoefKO :: Int -> Int -> IO (Maybe CoefKO)
getCoefKO linkId koId = do
    coefs <- getFilterData "database/CoefKO.hdb" 0 10000 "" (\_ -> "") (\coef -> companyPolicyLinkId coef == linkId && typeKOId coef == koId)
    case (length coefs) of 
        0 -> return Nothing
        _ -> return $ Just (coefs !! (0))