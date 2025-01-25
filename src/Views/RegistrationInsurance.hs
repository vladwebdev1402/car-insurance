module Views.RegistrationInsurance (registrationInsurance) where
import Views.InputOsagoData
import Views.InputKaskoData
import Views.CalcOsagoPrices
import Views.CalcKaskoPrices
import Modules.ChoosePolicyType
import Shared.Inputs.ChooseData
import Shared.Helpers.GetTodayDate
import Shared.Helpers.GetCountDays
import Enteties.PolicyTypes
import Enteties.Policies
import Enteties.Companys
import Enteties.Deductibles
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
    _ -> return ()

registrationOsago :: Maybe OsagoUserInfo -> Int -> IO ()
registrationOsago oldOsagoUserInfo editPunkt = do

  let osagoUserInfo = case oldOsagoUserInfo of
        Nothing -> nullOsagoUserInfo
        _ -> osagoUserInfo

  osagoUserInfo <- inputOsagoData osagoUserInfo editPunkt True ""

  case (Views.InputOsagoData.birthDate osagoUserInfo) of
    Nothing -> return ()
    _ -> do
        companysWithPrices <- calcOsagoPrices osagoUserInfo

        index <- chooseData companysWithPrices (\array -> generateLogData array (\(company, price) -> (Enteties.Companys.name company) ++ " - " ++ (printf "%.2f" price))) "\nВыберите компанию. Чтобы выйти, введите \"выход\"" ""

        if (index == -1) then return ()
        else do 
          let (company, price) = (companysWithPrices !! (index - 1))
          date <- getTodayDate
          let policyTypeId = 0
          let (_, _, _, _, _, certificate) = nothingToJust (Views.InputOsagoData.autoInfo osagoUserInfo) "registrationOsago: error get auto info"
          let cert = nothingToJust certificate "registrationOsago: error get certificate"
          let typeKs = nothingToJust (Views.InputOsagoData.typeKS osagoUserInfo) "registrationOsago: error get count days "
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
            Enteties.Policies.date = date
          }

          addNewPolicy newPolicy

          return ()

  case (Views.InputOsagoData.birthDate osagoUserInfo) of
    Nothing -> return ()
    _ -> putStrLn "Регистрация ОСАГО: Функция в разработке."

registrationKasko :: Maybe KaskoUserInfo -> Int -> IO ()
registrationKasko oldKaskoUserInfo editPunkt = do
  let kaskoUserInfo = case oldKaskoUserInfo of
        Nothing -> nullKaskoUserInfo
        _ -> kaskoUserInfo

  kaskoUserInfo <- inputKaskoData kaskoUserInfo editPunkt True ""

  case (Views.InputKaskoData.birthDate kaskoUserInfo) of
    Nothing -> return ()
    _ -> do 
      companysWithPrices <- calcKaskoPrices kaskoUserInfo

      index <- chooseData companysWithPrices (\array -> generateLogData array (\(company, price) -> (Enteties.Companys.name company) ++ " - " ++ (printf "%.2f" price))) "\nВыберите компанию. Чтобы выйти, введите \"выход\"" ""
      
      if (index == -1) then return ()
      else do
          let (company, price) = (companysWithPrices !! (index - 1))
          
          date <- getTodayDate
          
          let policyTypeId = 1
          
          let (_, _, _, _, _, certificate) = nothingToJust (Views.InputKaskoData.autoInfo kaskoUserInfo) "registrationKasko: error get auto info"
          
          let cert = nothingToJust certificate "registrationKasko: error get certificate"

          let typeKs = nothingToJust (Views.InputKaskoData.typeKS kaskoUserInfo) "registrationKasko: error get typeKs "

          let deductible = nothingToJust (Views.InputKaskoData.deductible kaskoUserInfo) "registrationKasko: error get deductible "

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
            Enteties.Policies.sumDeductible = 0.0,
            Enteties.Policies.date = date
          }

          addNewPolicy newPolicy

          putStrLn "Регистрация КАСКО: Функция в разработке."