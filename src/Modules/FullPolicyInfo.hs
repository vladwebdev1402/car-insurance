module Modules.FullPolicyInfo (FullPolicyInfo(..), getFullInfoForPolicy, getFullInfoForPolicyString) where

import Text.Printf (printf)
import Entities.TransportCertificate
import Entities.Policies
import Entities.CompanyPolicyLink
import Entities.Companys
import Entities.PolicyTypes
import Entities.PolicyCase

data FullPolicyInfo = FullPolicyInfo { policy :: Policy,
    certificate :: TransportCertificate,
    link :: CompanyPolicyLink,
    company :: Company,
    policyType :: PolicyType,
    policyCases :: [PolicyCase]
} deriving (Show, Read)

getFullInfoForPolicyString :: FullPolicyInfo -> String
getFullInfoForPolicyString fullPolicyInfo =
    "\nТип полиса: " ++ (Entities.PolicyTypes.name (policyType fullPolicyInfo)) ++
    "\nКомпания: " ++ (Entities.Companys.name (company fullPolicyInfo)) ++ 
    "\nРегистрационный номер автомобиля: " ++ (Entities.TransportCertificate.registrationNumber (certificate fullPolicyInfo)) ++ 
    "\nСтоимость оформления: " ++ (printf "%.2f"  (Entities.Policies.sumInsurance (policy fullPolicyInfo))) ++ 
    (case (Entities.Policies.sumDeductible (policy fullPolicyInfo)) of 
        0.0 -> ""
        sum -> "\nРазмер франшизы: " ++ (printf "%.2f" sum)
        ) ++ 
    (case (Entities.Policies.sumRemaininInsurance (policy fullPolicyInfo)) of 
        0.0 -> ""
        sum -> "\nСумма возврата: " ++ (printf "%.2f" sum)
        ) ++ 
    "\nДата оформления: " ++ (Entities.Policies.date (policy fullPolicyInfo)) ++ 
    "\nСрок оформления: " ++ (show (Entities.Policies.countDays (policy fullPolicyInfo))) ++ " дней" ++ 
    "\nСтатус полиса: " ++ (policyStatusRuTranslate (Entities.Policies.status (policy fullPolicyInfo))) ++
    getPolicyCasesInfo (policyCases fullPolicyInfo)

getFullInfoForPolicy :: [(TransportCertificate, Policy)] -> IO [FullPolicyInfo]
getFullInfoForPolicy certificatesWithPolicy = do
    fullPolicies <- mapM (\(cert, policy) -> do
        policyType <- getPolicyTypeById (Entities.Policies.policyTypeId policy) 

        link <- getCompanyPolicyLinkById (Entities.Policies.companyPolicyLinkId policy) 

        company <- getCompanyById (Entities.CompanyPolicyLink.companyId link)

        policyCase <- getPolicyCases (Entities.Policies.uid policy)

        return (FullPolicyInfo policy cert link company policyType policyCase)
        ) certificatesWithPolicy

    return fullPolicies