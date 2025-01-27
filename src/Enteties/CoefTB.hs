module Enteties.CoefTB (CoefTB(..), getCoefTB) where

import Shared.Api.GetFilterData

data CoefTB = CoefTB { uid :: Int, companyPolicyLinkId :: Int, typeTransportId :: Int, value :: Float } deriving (Read, Show)

getCoefTB :: Int -> Int -> IO (Maybe CoefTB)
getCoefTB linkId categoryId = do
    coefs <- getFilterData "database/CoefTB.hdb" 0 10000 "" (\_ -> "") (\coef -> companyPolicyLinkId coef == linkId && typeTransportId coef == categoryId)
    case (length coefs) of 
        0 -> return Nothing
        _ -> return $ Just (coefs !! (0))