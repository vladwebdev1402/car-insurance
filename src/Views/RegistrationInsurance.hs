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
import Entities.PolicyTypes
import Entities.Policies
import Entities.Companys
import Entities.Deductibles
import Entities.Additional
import Entities.TypeKS
import Entities.CompanyPolicyLink
import Entities.TransportCertificate
import Shared.Logs.LogData
import Shared.Logs.FormateNumber
import Shared.Validators.NothingToJust

registrationInsurance :: IO ()
registrationInsurance = do
  policyType <- choosePolicyType


  case (Entities.PolicyTypes.uid policyType) of
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

        index <- chooseData companysWithPrices (\array -> generateLogData array (\(company, price) -> (Entities.Companys.name company) ++ " - " ++ (formateFloat price))) "\nВыберите компанию. Чтобы выйти, введите \"выход\"" ""

        if (index == -1) then return ()
        else do 
          let (company, price) = (companysWithPrices !! (index - 1))
          date <- getTodayDate
          let policyTypeId = 0
          let (_, _, _, _, _, certificate) = nothingToJust (Views.UserInfo.autoInfo osagoUserInfo) "registrationOsago: error get auto info"
          let cert = nothingToJust certificate "registrationOsago: error get certificate"
          let typeKs = nothingToJust (Views.UserInfo.typeKS osagoUserInfo) "registrationOsago: error get count days "
          let countDays = getCountDaysFromMonths (Entities.TypeKS.countMonths typeKs)
          companyLink <- getCompanyPolicyLinkByCompany (Entities.Companys.uid company) policyTypeId

          let newPolicy = Policy {
            Entities.Policies.uid = 0,
            Entities.Policies.companyPolicyLinkId = (Entities.CompanyPolicyLink.uid companyLink),
            Entities.Policies.policyTypeId = policyTypeId,
            Entities.Policies.transportCertificateId = Entities.TransportCertificate.uid cert,
            Entities.Policies.status = "active",
            Entities.Policies.countDays = countDays,
            Entities.Policies.sumInsurance = price,
            Entities.Policies.sumRemaininInsurance = 0.0,
            Entities.Policies.sumDeductible = 0.0,
            Entities.Policies.sumAdditional = 0.0,
            Entities.Policies.date = date
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

      index <- chooseData companysWithPrices (\array -> generateLogData array (\(company, price) -> (Entities.Companys.name company) ++ " - " ++ (formateFloat price))) "\nВыберите компанию. Чтобы выйти, введите \"выход\"" ""
      
      if (index == -1) then return ()
      else do
          let (company, price) = (companysWithPrices !! (index - 1))
          
          date <- getTodayDate
          
          let policyTypeId = 1
          
          let (_, _, _, _, _, certificate) = nothingToJust (Views.UserInfo.autoInfo kaskoUserInfo) "registrationKasko: error get auto info"
          
          let cert = nothingToJust certificate "registrationKasko: error get certificate"

          let typeKs = nothingToJust (Views.UserInfo.typeKS kaskoUserInfo) "registrationKasko: error get typeKs "

          let deductible = nothingToJust (Views.UserInfo.deductible kaskoUserInfo) "registrationKasko: error get deductible "

          let countDeductible = (Entities.Deductibles.sumDeductible deductible) 

          let countDays = getCountDaysFromMonths (Entities.TypeKS.countMonths typeKs)

          companyLink <- getCompanyPolicyLinkByCompany (Entities.Companys.uid company) policyTypeId

          let newPolicy = Policy {
            Entities.Policies.uid = 0,
            Entities.Policies.companyPolicyLinkId = (Entities.CompanyPolicyLink.uid companyLink),
            Entities.Policies.policyTypeId = policyTypeId,
            Entities.Policies.transportCertificateId = Entities.TransportCertificate.uid cert,
            Entities.Policies.status = "active",
            Entities.Policies.countDays = countDays,
            Entities.Policies.sumInsurance = price,
            Entities.Policies.sumRemaininInsurance = 0.0,
            Entities.Policies.sumDeductible = countDeductible,
            Entities.Policies.sumAdditional = 0.0,
            Entities.Policies.date = date
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

      index <- chooseData companysWithPrices (\array -> generateLogData array (\(company, price) -> (Entities.Companys.name company) ++ " - " ++ (formateFloat price))) "\nВыберите компанию. Чтобы выйти, введите \"выход\"" ""
      
      if (index == -1) then return ()
      else do
          let (company, price) = (companysWithPrices !! (index - 1))
          
          date <- getTodayDate
          
          let policyTypeId = 2
          
          let (_, _, _, _, _, certificate) = nothingToJust (Views.UserInfo.autoInfo dsagoUserInfo) "registrationDsago: error get auto info"
          
          let cert = nothingToJust certificate "registrationDsago: error get certificate"

          let typeKs = nothingToJust (Views.UserInfo.typeKS dsagoUserInfo) "registrationDsago: error get typeKs "

          let additional = nothingToJust (Views.UserInfo.additional dsagoUserInfo) "registrationDsago: error get additional "

          let countDays = getCountDaysFromMonths (Entities.TypeKS.countMonths typeKs)

          companyLink <- getCompanyPolicyLinkByCompany (Entities.Companys.uid company) policyTypeId

          let newPolicy = Policy {
            Entities.Policies.uid = 0,
            Entities.Policies.companyPolicyLinkId = (Entities.CompanyPolicyLink.uid companyLink),
            Entities.Policies.policyTypeId = policyTypeId,
            Entities.Policies.transportCertificateId = Entities.TransportCertificate.uid cert,
            Entities.Policies.status = "active",
            Entities.Policies.countDays = countDays,
            Entities.Policies.sumInsurance = price,
            Entities.Policies.sumRemaininInsurance = 0.0,
            Entities.Policies.sumDeductible = 0.0,
            Entities.Policies.sumAdditional = (Entities.Additional.value additional),
            Entities.Policies.date = date
          }

          addNewPolicy newPolicy

          return ()

  return ()
