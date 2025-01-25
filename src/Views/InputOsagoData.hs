module Views.InputOsagoData (OsagoUserInfo(..), inputOsagoData, nullOsagoUserInfo) where

import System.Process (callCommand)
import Enteties.Regions
import Enteties.Territories
import Enteties.TypeKS
import Enteties.TypeKO
import Enteties.TransportBrands 
import Enteties.TransportModels 
import Enteties.Transports
import Enteties.TypesTransport
import Enteties.TransportCertificate
import Enteties.Drivers
import Enteties.Policies
import Shared.Inputs.InputDayOfBirth
import Shared.Inputs.InputRangeNumber
import Shared.Calc.CalcAgeFromDate
import Shared.Logs.LogData
import Shared.Validators.NothingToJust
import Shared.Calc.GetMaximumDrivingExpirience
import Modules.ChooseRegion
import Modules.ChooseTerritorie
import Modules.ChoosePolicyType
import Modules.ChooseTypeKS
import Modules.ChooseTypeKO
import Modules.InputUserPassport
import Views.Helpers.InputAutoInfo
import Views.Helpers.GetAutoInfo
import Views.Helpers.ChooseOsagoEditStep
import Views.Helpers.ConfirmIdentity

data OsagoUserInfo = OsagoUserInfo {
  birthDate :: Maybe (Int, String),
  drivingExpirience :: Maybe Int,
  autoInfo :: Maybe (Int, Maybe TransportBrand, Maybe TransportModel, Maybe Transport, TypeTransport, Maybe TransportCertificate),
  region :: Maybe Region,
  territorie :: Maybe Territorie,
  typeKS :: Maybe TypeKS,
  typeKO :: Maybe TypeKO}

nullOsagoUserInfo :: OsagoUserInfo
nullOsagoUserInfo = OsagoUserInfo {Views.InputOsagoData.birthDate = Nothing, 
                             Views.InputOsagoData.drivingExpirience = Nothing,
                             Views.InputOsagoData.autoInfo = Nothing, 
                             Views.InputOsagoData.region = Nothing, 
                             Views.InputOsagoData.territorie = Nothing, 
                             Views.InputOsagoData.typeKS = Nothing, 
                             Views.InputOsagoData.typeKO = Nothing}

inputOsagoData :: OsagoUserInfo -> Int -> Bool -> String -> IO OsagoUserInfo
inputOsagoData osagoUserInfo editStep False _ = do
  callCommand "cls" 

  (age, birthDate) <- if editStep == 1
    then inputDayOfBirth 16 100 
    else maybe (inputDayOfBirth 16 100) return (Views.InputOsagoData.birthDate osagoUserInfo)

  let infoMessage1 = "Выбран тип страховки: ОСАГО\nДата рождения: " ++ birthDate ++ "\nВозраст: " ++ show age

  callCommand "cls" 
  drivingExpirience <- if editStep == 1
    then inputRangeNumber infoMessage1 "Введите стаж вождения: " 0 (getMaximumDrivingExpirience age) 
    else maybe (inputRangeNumber infoMessage1 "Введите стаж вождения: " 0 (getMaximumDrivingExpirience age)) return (drivingExpirience osagoUserInfo)

  let infoMessage2 = infoMessage1 ++ "\nСтаж вождения: " ++ show drivingExpirience

  callCommand "cls" 
  (enginePower, transportBrand, transportModel, transport, category, _) <- if editStep == 2
    then inputAutoInfo False infoMessage2 
    else maybe (inputAutoInfo False infoMessage2) return (autoInfo osagoUserInfo)

  let infoMessage3 = infoMessage2 ++ (getAutoInfo enginePower transportBrand transportModel transport category Nothing)

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
  editPunkt <- chooseOsagoEditStep False infoMessage7
  
  let osagoInfo = OsagoUserInfo {Views.InputOsagoData.birthDate = Just (age, birthDate), 
          drivingExpirience = Just drivingExpirience,
          autoInfo = Just (enginePower, transportBrand, transportModel, transport, category, Nothing), region = Just region, territorie = Just territorie, typeKS = Just typeKs, typeKO = Just typeKo}

  case editPunkt of 
    (-1) -> return osagoInfo
    _ -> inputOsagoData osagoInfo (editPunkt) False ""

inputOsagoData osagoUserInfo editStep True errorMessage = do

  callCommand "cls" 
  putStrLn errorMessage
  (enginePower, transportBrand, transportModel, transport, category, certificate) <- if editStep == 1
    then inputAutoInfo True errorMessage
    else maybe (inputAutoInfo True errorMessage) return (autoInfo osagoUserInfo)

  let cert = nothingToJust certificate "inputOsagoData: ошибка получение транспортного сертификата"

  activeOsago <- getActivePolicy (Enteties.TransportCertificate.uid cert) 0

  case activeOsago of
    Nothing -> do 
        driver <- getDriverById (Enteties.TransportCertificate.driverId cert) 

        isSusscessfulIdentification <- if editStep == 1 || editStep == -1
          then confirmIdentity driver
          else return True

        if not (isSusscessfulIdentification) then return nullOsagoUserInfo
        else do 
          age <- calcAgeFromDate (Enteties.Drivers.birthday driver)

          let infoMessage1 = "\nВыбран тип страховки для оформления: ОСАГО" ++
                            getAutoInfo enginePower transportBrand transportModel transport category (Just cert)

          region <- if editStep == 2
          then chooseRegion infoMessage1 
          else maybe (chooseRegion infoMessage1)  return (region osagoUserInfo)

          let infoMessage2 = infoMessage1 ++ "\nРегион: " ++ (Enteties.Regions.name region)

          territorie <- if editStep == 2
            then chooseTerritorie (Enteties.Regions.uid region) infoMessage2 
            else maybe (chooseTerritorie (Enteties.Regions.uid region) infoMessage2) return (territorie osagoUserInfo)

          let infoMessage3 = infoMessage2 ++ "\nМесто проживания: " ++ (Enteties.Territories.name territorie)

          typeKs <- if editStep == 3
          then сhooseTypeKS infoMessage3 
          else maybe (сhooseTypeKS infoMessage3) return (typeKS osagoUserInfo)

          let infoMessage4 = infoMessage3 ++ "\nСрок страхования: " ++ (Enteties.TypeKS.description typeKs)

          typeKo <- if editStep == 4
          then сhooseTypeKO infoMessage4 
          else maybe (сhooseTypeKO infoMessage4) return (typeKO osagoUserInfo)

          let infoMessage5 = infoMessage4 ++ "\nКоличество водителей: " ++ (Enteties.TypeKO.description typeKo)

          let osagoInfo = OsagoUserInfo {Views.InputOsagoData.birthDate = Just (age, (Enteties.Drivers.birthday driver)),
              drivingExpirience = Just (Enteties.Drivers.experience driver),
              autoInfo = Just (enginePower, transportBrand, transportModel, transport, category, (Just cert)), 
              region = (Just region), 
              territorie = (Just territorie),
              typeKS = (Just typeKs), 
              typeKO = (Just typeKo)}

          editPunkt <- chooseOsagoEditStep True infoMessage5

          case editPunkt of 
              (-1) -> return osagoInfo
              _ -> inputOsagoData osagoInfo editPunkt True ""
  
    _ -> do
        inputOsagoData (OsagoUserInfo {Views.InputOsagoData.birthDate = Nothing, 
                             Views.InputOsagoData.drivingExpirience = Nothing,
                             Views.InputOsagoData.autoInfo = Nothing, 
                             Views.InputOsagoData.region = Views.InputOsagoData.region osagoUserInfo, 
                             Views.InputOsagoData.territorie = Views.InputOsagoData.territorie osagoUserInfo, 
                             Views.InputOsagoData.typeKS = Views.InputOsagoData.typeKS osagoUserInfo, 
                             Views.InputOsagoData.typeKO = Views.InputOsagoData.typeKO osagoUserInfo} ) (-1) True "На данный автомобиль уже зарегестрирован полис ОСАГО"

  

 
  
