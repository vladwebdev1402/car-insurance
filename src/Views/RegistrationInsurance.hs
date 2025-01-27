module Views.RegistrationInsurance (registrationInsurance) where
import Views.InputOsagoData
import Views.InputKaskoData
import Views.InputDsagoData
import Views.CalcOsagoPrices
import Views.CalcDsagoPrices
import Views.CalcKaskoPrices
import Views.UserInfo
import Modules.ChoosePolicyType
import Shared.Inputs.ChooseData
import Shared.Helpers.GetTodayDate
import Shared.Helpers.GetCountDays
import Enteties.PolicyTypes
import Enteties.Policies
import Enteties.Companys
import Enteties.Deductibles
import Enteties.Additional
import Enteties.TypeKS
import Enteties.CompanyPolicyLink
import Enteties.TransportCertificate
import Shared.Logs.LogData
import Shared.Validators.NothingToJust
import Text.Printf (printf)

registrationInsurance :: IO ()
registrationInsurance = do
  policyType <- choosePolicyType


  case (Enteties.PolicyTypes.uid policyType) of
    0 -> registrationOsago Nothing (-1) 
    1 -> registrationKasko Nothing (-1) 
    2 -> registrationDsago Nothing (-1) 
    _ -> return ()

registrationOsago :: Maybe UserInfo -> Int -> IO ()
registrationOsago oldOsagoUserInfo editPunkt = do

  let osagoUserInfo = case oldOsagoUserInfo of
        Nothing -> nullUserInfo
        Just osagoInfo -> osagoInfo

  osagoUserInfo <- inputOsagoData osagoUserInfo editPunkt True ""

  case (Views.UserInfo.birthDate osagoUserInfo) of
    Nothing -> return ()
    _ -> do
        companysWithPrices <- calcOsagoPrices osagoUserInfo

        index <- chooseData companysWithPrices (\array -> generateLogData array (\(company, price) -> (Enteties.Companys.name company) ++ " - " ++ (printf "%.2f" price))) "\nВыберите компанию. Чтобы выйти, введите \"выход\"" ""

        if (index == -1) then return ()
        else do 
          let (company, price) = (companysWithPrices !! (index - 1))
          date <- getTodayDate
          let policyTypeId = 0
          let (_, _, _, _, _, certificate) = nothingToJust (Views.UserInfo.autoInfo osagoUserInfo) "registrationOsago: error get auto info"
          let cert = nothingToJust certificate "registrationOsago: error get certificate"
          let typeKs = nothingToJust (Views.UserInfo.typeKS osagoUserInfo) "registrationOsago: error get count days "
          let countDays = getCountDaysFromMonths (Enteties.TypeKS.countMonths typeKs)
          companyLink <- getCompanyPolicyLinkByCompany (Enteties.Companys.uid company) policyTypeId

          let newPolicy = Policy {
            Enteties.Policies.uid = 0,
            Enteties.Policies.companyPolicyLinkId = (Enteties.CompanyPolicyLink.uid companyLink),
            Enteties.Policies.policyTypeId = policyTypeId,
            Enteties.Policies.transportCertificateId = Enteties.TransportCertificate.uid cert,
            Enteties.Policies.status = "active",
            Enteties.Policies.countDays = countDays,
            Enteties.Policies.sumInsurance = price,
            Enteties.Policies.sumRemaininInsurance = 0.0,
            Enteties.Policies.sumDeductible = 0.0,
            Enteties.Policies.sumAdditional = 0.0,
            Enteties.Policies.date = date
          }

          addNewPolicy newPolicy

          return ()

registrationKasko :: Maybe UserInfo -> Int -> IO ()
registrationKasko oldKaskoUserInfo editPunkt = do
  let kaskoUserInfo = case oldKaskoUserInfo of
        Nothing -> nullUserInfo
        Just kaskoInfo -> kaskoInfo

  kaskoUserInfo <- inputKaskoData kaskoUserInfo editPunkt True ""

  case (Views.UserInfo.birthDate kaskoUserInfo) of
    Nothing -> return ()
    _ -> do 
      companysWithPrices <- calcKaskoPrices kaskoUserInfo

      index <- chooseData companysWithPrices (\array -> generateLogData array (\(company, price) -> (Enteties.Companys.name company) ++ " - " ++ (printf "%.2f" price))) "\nВыберите компанию. Чтобы выйти, введите \"выход\"" ""
      
      if (index == -1) then return ()
      else do
          let (company, price) = (companysWithPrices !! (index - 1))
          
          date <- getTodayDate
          
          let policyTypeId = 1
          
          let (_, _, _, _, _, certificate) = nothingToJust (Views.UserInfo.autoInfo kaskoUserInfo) "registrationKasko: error get auto info"
          
          let cert = nothingToJust certificate "registrationKasko: error get certificate"

          let typeKs = nothingToJust (Views.UserInfo.typeKS kaskoUserInfo) "registrationKasko: error get typeKs "

          let deductible = nothingToJust (Views.UserInfo.deductible kaskoUserInfo) "registrationKasko: error get deductible "

          let countDeductible = (Enteties.Deductibles.sumDeductible deductible) 

          let countDays = getCountDaysFromMonths (Enteties.TypeKS.countMonths typeKs)

          companyLink <- getCompanyPolicyLinkByCompany (Enteties.Companys.uid company) policyTypeId

          let newPolicy = Policy {
            Enteties.Policies.uid = 0,
            Enteties.Policies.companyPolicyLinkId = (Enteties.CompanyPolicyLink.uid companyLink),
            Enteties.Policies.policyTypeId = policyTypeId,
            Enteties.Policies.transportCertificateId = Enteties.TransportCertificate.uid cert,
            Enteties.Policies.status = "active",
            Enteties.Policies.countDays = countDays,
            Enteties.Policies.sumInsurance = price,
            Enteties.Policies.sumRemaininInsurance = 0.0,
            Enteties.Policies.sumDeductible = countDeductible,
            Enteties.Policies.sumAdditional = countDeductible,
            Enteties.Policies.date = date
          }

          addNewPolicy newPolicy

          return ()

registrationDsago :: Maybe UserInfo -> Int -> IO ()
registrationDsago oldDsagoUserInfo editPunkt = do
  let dsagoUserInfo = case oldDsagoUserInfo of
        Nothing -> nullUserInfo
        Just dsagoInfo -> dsagoInfo

  dsagoUserInfo <- inputDsagoData dsagoUserInfo editPunkt True ""

  case (Views.UserInfo.birthDate dsagoUserInfo) of
    Nothing -> return ()
    _ -> do 
      companysWithPrices <- calcDsagoPrices dsagoUserInfo

      index <- chooseData companysWithPrices (\array -> generateLogData array (\(company, price) -> (Enteties.Companys.name company) ++ " - " ++ (printf "%.2f" price))) "\nВыберите компанию. Чтобы выйти, введите \"выход\"" ""
      
      if (index == -1) then return ()
      else do
          let (company, price) = (companysWithPrices !! (index - 1))
          
          date <- getTodayDate
          
          let policyTypeId = 1
          
          let (_, _, _, _, _, certificate) = nothingToJust (Views.UserInfo.autoInfo dsagoUserInfo) "registrationDsago: error get auto info"
          
          let cert = nothingToJust certificate "registrationDsago: error get certificate"

          let typeKs = nothingToJust (Views.UserInfo.typeKS dsagoUserInfo) "registrationDsago: error get typeKs "

          let additional = nothingToJust (Views.UserInfo.additional dsagoUserInfo) "registrationDsago: error get additional "

          let countDays = getCountDaysFromMonths (Enteties.TypeKS.countMonths typeKs)

          companyLink <- getCompanyPolicyLinkByCompany (Enteties.Companys.uid company) policyTypeId

          let newPolicy = Policy {
            Enteties.Policies.uid = 0,
            Enteties.Policies.companyPolicyLinkId = (Enteties.CompanyPolicyLink.uid companyLink),
            Enteties.Policies.policyTypeId = policyTypeId,
            Enteties.Policies.transportCertificateId = Enteties.TransportCertificate.uid cert,
            Enteties.Policies.status = "active",
            Enteties.Policies.countDays = countDays,
            Enteties.Policies.sumInsurance = price,
            Enteties.Policies.sumRemaininInsurance = 0.0,
            Enteties.Policies.sumDeductible = 0.0,
            Enteties.Policies.sumAdditional = (Enteties.Additional.value additional),
            Enteties.Policies.date = date
          }

          addNewPolicy newPolicy

          return ()

  return ()
