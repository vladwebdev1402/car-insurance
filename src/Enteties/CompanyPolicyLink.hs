module Enteties.CompanyPolicyLink (CompanyPolicyLink(..), getCompanyPolicyLinkByPolicy) where

import Shared.Api.GetFilterData

data CompanyPolicyLink = CompanyPolicyLink { uid :: Int, companyId :: Int, policyTypeId :: Int, amountPercent :: Int } deriving (Read, Show)

getCompanyPolicyLinkByPolicy :: Int -> IO [CompanyPolicyLink]
getCompanyPolicyLinkByPolicy policyId = getFilterData "database/CompanyPolicyLink.hdb" 0 10000 "" (\x -> "") (\link -> policyTypeId link == policyId)