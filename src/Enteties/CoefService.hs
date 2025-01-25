module Enteties.CoefService (CoefService(..), getCoefService, getSummuryCoefService) where

import Shared.Api.GetFilterData
import Enteties.PolicyServices

data CoefService = CoefService { uid :: Int, serviceId :: Int, companyPolicyLinkId :: Int, value :: Float } deriving (Read, Show)

getCoefService :: Int -> Int -> IO CoefService
getCoefService linkId servId = do
    coefs <- getFilterData "database/CoefService.hdb" 0 10000 "" (\_ -> "") (\coef -> companyPolicyLinkId coef == linkId && serviceId coef == servId)
    return $ coefs !! (0)

getSummuryCoefService :: Int -> [PolicyService] -> IO Float
getSummuryCoefService linkId services = do
    coefs <- mapM (\item -> getCoefService linkId (Enteties.PolicyServices.uid item)) services
    let values = map Enteties.CoefService.value coefs
    return (product values)