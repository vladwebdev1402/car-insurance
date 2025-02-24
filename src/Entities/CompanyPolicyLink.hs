module Entities.CompanyPolicyLink (CompanyPolicyLink(..), getCompanyPolicyLinkByPolicy, getCompanyPolicyLinkById, getCompanyPolicyLinkByCompany) where

import Shared.Api.GetFilterData

data CompanyPolicyLink = CompanyPolicyLink { uid :: Int, 
        companyId :: Int, 
        policyTypeId :: Int, 
        amountPercent :: Float } deriving (Read, Show)

getCompanyPolicyLinkByPolicy :: Int -> IO [CompanyPolicyLink]
getCompanyPolicyLinkByPolicy policyId = getFilterData "database/CompanyPolicyLink.hdb" 0 10000 "" (\x -> "") (\link -> policyTypeId link == policyId)

getCompanyPolicyLinkById :: Int -> IO CompanyPolicyLink
getCompanyPolicyLinkById linkUid = do 
    links <- getFilterData "database/CompanyPolicyLink.hdb" 0 10000 "" (\x -> "") (\link -> uid link == linkUid)
    return $ links !! (0)

getCompanyPolicyLinkByCompany :: Int -> Int -> IO CompanyPolicyLink
getCompanyPolicyLinkByCompany company typeId = do
    links <- getFilterData "database/CompanyPolicyLink.hdb" 0 10000 "" (\x -> "") (\link -> policyTypeId link == typeId && companyId link == company)
    return $ links !! (0)