module Enteties.CoefService (CoefService(..), getCoefService, getSummuryCoefService) where

import Shared.Api.GetFilterData
import Enteties.PolicyServices

data CoefService = CoefService { uid :: Int, serviceId :: Int, companyPolicyLinkId :: Int, value :: Float } deriving (Read, Show)

getCoefService :: Int -> Int -> IO (Maybe CoefService)
getCoefService linkId servId = do
    coefs <- getFilterData "database/CoefService.hdb" 0 10000 "" (\_ -> "") (\coef -> companyPolicyLinkId coef == linkId && serviceId coef == servId)
    case (length coefs) of 
        0 -> return Nothing
        _ -> return $ Just (coefs !! (0))

getSummuryCoefService :: Int -> [PolicyService] -> IO Float
getSummuryCoefService linkId services = do
    coefs <- mapM (\item -> getCoefService linkId (Enteties.PolicyServices.uid item)) services

    let maybeCoefs = sequence coefs

    return $ maybe 0 (product . map Enteties.CoefService.value) maybeCoefs