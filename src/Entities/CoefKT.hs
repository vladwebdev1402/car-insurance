module Entities.CoefKT (CoefKT(..), getCoefKT) where

import Shared.Api.GetFilterData

data CoefKT = CoefKT { uid :: Int, companyPolicyLinkId :: Int, territorieId :: Int, value :: Float } deriving (Read, Show)

getCoefKT :: Int -> Int -> IO (Maybe CoefKT)
getCoefKT linkId terId = do
    coefs <- getFilterData "database/CoefKT.hdb" 0 10000 "" (\_ -> "") (\coef -> companyPolicyLinkId coef == linkId && territorieId coef == terId)
    case (length coefs) of 
        0 -> return Nothing
        _ -> return $ Just (coefs !! (0))