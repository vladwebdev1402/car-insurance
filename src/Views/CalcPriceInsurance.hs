module Views.CalcPriceInsurance (calcPriceInsurance) where

import Views.InputOsagoData 
import Enteties.PolicyTypes
import Enteties.TypesKVS
import Enteties.TypesKM
import Enteties.TypesKBM
import Enteties.TypeKS
import Enteties.TypeKO
import Enteties.CompanyPolicyLink
import Enteties.Territories
import Modules.ChoosePolicyType

import System.Process (callCommand)

getOsagoCoeffs :: OsagoUserInfo -> IO ()
getOsagoCoeffs osagoUserInfo = do
  let policyId = 0
  links <- getCompanyPolicyLinkByPolicy policyId

  (age, date) <- case Views.InputOsagoData.birthDate osagoUserInfo of
    Nothing -> error "getOsagoCoeffs: Ошибка получения возраста и даты"
    Just (age, date) -> return (age, date)

  drivingExpirience <- case Views.InputOsagoData.drivingExpirience osagoUserInfo of
    Nothing -> error "getOsagoCoeffs: Ошибка получения опыта вождения"
    Just drivingExpirience -> return drivingExpirience

  territorie <- case Views.InputOsagoData.territorie osagoUserInfo of
    Nothing -> error "getOsagoCoeffs: Ошибка места проживания"
    Just territorie -> return territorie

  typeKs <- case Views.InputOsagoData.typeKS osagoUserInfo of
    Nothing -> error "getOsagoCoeffs: Ошибка получения срока страхования"
    Just typeKs -> return typeKs
  
  typeKo <- case Views.InputOsagoData.typeKO osagoUserInfo of
    Nothing -> error "getOsagoCoeffs: Ошибка получения количества водителей"
    Just typeKo -> return typeKo
  
  autoInfo <- case Views.InputOsagoData.autoInfo osagoUserInfo of
    Nothing -> error "getOsagoCoeffs: Ошибка информации об автомобиле"
    Just autoInfo -> return autoInfo

  let (enginePower, transportBrand, transportModel, transport, category) = autoInfo

  coefKVS <- getTypeKVS age drivingExpirience True
  let coefKT = (Enteties.Territories.coefOsago territorie)
  coefKM <- getTypeKMByPower enginePower True
  coefKBM <- getTypeKBMByDriverLever 0

  -- typeKs, typeKo, coefKVS, coefKT, coefKM, coefKBM

  putStrLn "конец расчёта"



calcPriceInsurance :: IO ()
calcPriceInsurance = do
  policyType <- choosePolicyType

  case (Enteties.PolicyTypes.uid policyType) of
    0 -> calcOsagoPrice False
    1 -> calcKaskoPrice
    _ -> return ()

calcOsagoPrice :: Bool -> IO ()
calcOsagoPrice isRegistration = do
  osagoUserInfo <- inputOsagoData OsagoUserInfo {birthDate = Nothing, drivingExpirience = Nothing,
          autoInfo = Nothing, region = Nothing, territorie = Nothing, typeKS = Nothing, typeKO = Nothing} (-1) isRegistration

  getOsagoCoeffs osagoUserInfo

  putStrLn "Программа"

calcKaskoPrice :: IO ()
calcKaskoPrice = do
  putStrLn $ "Выбран тип страховки: КАСКО"

  
