module Modules.FullPolicyInfo (FullPolicyInfo(..), getFullInfoForPolicy, getFullInfoForPolicyString) where

import Text.Printf (printf)
import Enteties.TransportCertificate
import Enteties.Policies
import Enteties.CompanyPolicyLink
import Enteties.Companys
import Enteties.PolicyTypes
import Enteties.PolicyCase

data FullPolicyInfo = FullPolicyInfo { policy :: Policy,
    certificate :: TransportCertificate,
    link :: CompanyPolicyLink,
    company :: Company,
    policyType :: PolicyType,
    policyCases :: [PolicyCase]
} deriving (Show, Read)

getFullInfoForPolicyString :: FullPolicyInfo -> String
getFullInfoForPolicyString fullPolicyInfo =
    "\nТип полиса: " ++ (Enteties.PolicyTypes.name (policyType fullPolicyInfo)) ++
    "\nКомпания: " ++ (Enteties.Companys.name (company fullPolicyInfo)) ++ 
    "\nРегистрационный номер автомобиля: " ++ (Enteties.TransportCertificate.registrationNumber (certificate fullPolicyInfo)) ++ 
    "\nСтоимость оформления: " ++ (printf "%.2f"  (Enteties.Policies.sumInsurance (policy fullPolicyInfo))) ++ 
    (case (Enteties.Policies.sumDeductible (policy fullPolicyInfo)) of 
        0.0 -> ""
        sum -> "\nРазмер франшизы: " ++ (printf "%.2f" sum)
        ) ++ 
    "\nДата оформления: " ++ (Enteties.Policies.date (policy fullPolicyInfo)) ++ 
    "\nСтатус полиса: " ++ (policyStatusRuTranslate (Enteties.Policies.status (policy fullPolicyInfo))) ++
    getPolicyCasesInfo (policyCases fullPolicyInfo)

getFullInfoForPolicy :: [(TransportCertificate, Policy)] -> IO [FullPolicyInfo]
getFullInfoForPolicy certificatesWithPolicy = do
    fullPolicies <- mapM (\(cert, policy) -> do
        policyType <- getPolicyTypeById (Enteties.Policies.policyTypeId policy) 

        link <- getCompanyPolicyLinkById (Enteties.Policies.companyPolicyLinkId policy) 

        company <- getCompanyById (Enteties.CompanyPolicyLink.companyId link)

        policyCase <- getPolicyCases (Enteties.Policies.uid policy)

        return (FullPolicyInfo policy cert link company policyType policyCase)
        ) certificatesWithPolicy

    return fullPolicies