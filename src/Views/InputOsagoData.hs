module Views.InputOsagoData (OsagoUserInfo(..), inputOsagoData) where

import System.Process (callCommand)
import Enteties.PolicyTypes 
import Enteties.Regions
import Enteties.Territories
import Enteties.TypeKS
import Enteties.TypeKO
import Enteties.TransportBrands 
import Enteties.TransportModels 
import Enteties.Transports
import Enteties.TypesTransport
import Shared.Inputs.ChooseData
import Shared.Inputs.InputDayOfBirth
import Shared.Inputs.InputRangeNumber
import Shared.Logs.LogData
import Shared.Calc.GetMaximumDrivingExpirience
import Modules.ChooseRegion
import Modules.ChooseTerritorie
import Modules.ChoosePolicyType
import Modules.ChooseTypeKS
import Modules.ChooseTypeKO
import Views.Helpers.InputAutoInfo
import Views.Helpers.GetAutoInfo

data OsagoUserInfo = OsagoUserInfo {
  birthDate :: Maybe (Int, String),
  drivingExpirience :: Maybe Int,
  autoInfo :: Maybe (Int, Maybe TransportBrand, Maybe TransportModel, Maybe Transport, TypeTransport),
  region :: Maybe Region,
  territorie :: Maybe Territorie,
  typeKS :: Maybe TypeKS,
  typeKO :: Maybe TypeKO}

inputOsagoData :: OsagoUserInfo -> Int -> Bool -> IO OsagoUserInfo
inputOsagoData osagoUserInfo editStep isRegistration = do
  callCommand "cls" 

  (age, birthDate) <- if editStep == 1
    then inputDayOfBirth 16 100 
    else maybe (inputDayOfBirth 16 100) return (birthDate osagoUserInfo)

  let infoMessage1 = "Выбран тип страховки: ОСАГО\nДата рождения: " ++ birthDate ++ "\nВозраст: " ++ show age

  callCommand "cls" 
  drivingExpirience <- if editStep == 1
    then inputRangeNumber infoMessage1 "Введите стаж вождения: " 0 (getMaximumDrivingExpirience age) 
    else maybe (inputRangeNumber infoMessage1 "Введите стаж вождения: " 0 (getMaximumDrivingExpirience age)) return (drivingExpirience osagoUserInfo)

  let infoMessage2 = infoMessage1 ++ "\nСтаж вождения: " ++ show drivingExpirience

  callCommand "cls" 
  (enginePower, transportBrand, transportModel, transport, category) <- if editStep == 2
    then inputAutoInfo isRegistration infoMessage2 
    else maybe (inputAutoInfo isRegistration infoMessage2) return (autoInfo osagoUserInfo)

  let infoMessage3 = infoMessage2 ++ (getAutoInfo enginePower transportBrand transportModel transport category)

  region <- if editStep == 3
    then chooseRegion infoMessage3 
    else maybe (chooseRegion infoMessage3)  return (region osagoUserInfo)

  let infoMessage4 = infoMessage3 ++ "\nРегион: " ++ (Enteties.Regions.name region)

  putStrLn infoMessage4
  territorie <- if editStep == 3
    then chooseTerritorie (Enteties.Regions.uid region) infoMessage4 
    else maybe (chooseTerritorie (Enteties.Regions.uid region) infoMessage4) return (territorie osagoUserInfo)

  let infoMessage5 = infoMessage4 ++ "\nМесто проживания: " ++ (Enteties.Territories.name territorie)

  typeKs <- if editStep == 4
    then сhooseTypeKS infoMessage5 
    else maybe (сhooseTypeKS infoMessage5) return (typeKS osagoUserInfo)

  let infoMessage6 = infoMessage5 ++ "\nСрок страхования: " ++ (Enteties.TypeKS.description typeKs)

  typeKo <- if editStep == 5
    then сhooseTypeKO infoMessage6 
    else maybe (сhooseTypeKO infoMessage6) return (typeKO osagoUserInfo)

  let infoMessage7 = infoMessage6 ++ "\nКоличество водителей: " ++ (Enteties.TypeKO.description typeKo)

  callCommand "cls" 
  let editPunkts = ["Продолжить", "Исправить дату рождения и опыт вождения", "Исправить информацию об автомобиле", "Исправить регион и место проживания", "Исправить срок страхования", "Исправить количество водителей"]
  
  editPunkt <- chooseData editPunkts (\array -> generateLogData array (\x -> x)) "Выберите пункт для дальнейшего действия: " infoMessage7
  
  let osagoInfo = OsagoUserInfo {birthDate = Just (age, birthDate), 
          drivingExpirience = Just drivingExpirience,
          autoInfo = Just (enginePower, transportBrand, transportModel, transport, category), region = Just region, territorie = Just territorie, typeKS = Just typeKs, typeKO = Just typeKo}

  case editPunkt of 
    1 -> return osagoInfo
    _ -> inputOsagoData osagoInfo (editPunkt - 1) isRegistration

  