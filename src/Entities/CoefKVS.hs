module Entities.CoefKVS (CoefKVS(..), getCoefKVS) where

import Shared.Api.GetFilterData

data CoefKVS = CoefKVS { uid :: Int, companyPolicyLinkId :: Int, typeKVSId :: Int, value :: Float } deriving (Read, Show)

getCoefKVS :: Int -> Int -> IO (Maybe CoefKVS)
getCoefKVS linkId kvsId = do
    coefs <- getFilterData "database/CoefKVS.hdb" 0 10000 "" (\_ -> "") (\coef -> companyPolicyLinkId coef == linkId && typeKVSId coef == kvsId)
    case (length coefs) of 
        0 -> return Nothing
        _ -> return $ Just (coefs !! (0))