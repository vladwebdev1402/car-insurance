module Enteties.CoefService (CoefService(..), getCoefService) where

import Shared.Api.GetFilterData

data CoefService = CoefService { uid :: Int, serviceId :: Int, companyPolicyLinkId :: Int, value :: Float } deriving (Read, Show)

getCoefService :: Int -> Int -> IO CoefService
getCoefService linkId servId = do
    coefs <- getFilterData "database/CoefService.hdb" 0 10000 "" (\_ -> "") (\coef -> companyPolicyLinkId coef == linkId && serviceId coef == servId)
    return $ coefs !! (0)