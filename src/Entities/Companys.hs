module Entities.Companys (Company(..), getCompanyById) where

import Shared.Api.GetFilterData

data Company = Company { uid :: Int, name :: String } deriving (Read, Show)

getCompanyById :: Int -> IO Company
getCompanyById companyId = do
    companys <- getFilterData "database/Companys.hdb" 0 10000 "" (\_ -> "") (\company -> uid company == companyId)
    return $ companys !! (0)
   