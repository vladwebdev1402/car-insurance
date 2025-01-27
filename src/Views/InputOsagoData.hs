module Views.InputOsagoData (inputOsagoData) where

import System.Process (callCommand)
import Entities.Regions
import Entities.Territories
import Entities.TypeKS
import Entities.TypeKO
import Entities.TransportBrands 
import Entities.TransportModels 
import Entities.Transports
import Entities.TypesTransport
import Entities.TransportCertificate
import Entities.Drivers
import Entities.Policies
import Shared.Inputs.InputDayOfBirth
import Shared.Inputs.InputRangeNumber
import Shared.Calc.CalcAgeFromDate
import Shared.Calc.GetMaximumDrivingExpirience
import Modules.ChooseRegion
import Modules.ChooseTerritorie
import Modules.ChooseTypeKS
import Modules.ChooseTypeKO
import Views.UserInfo
import Views.Helpers.InputAutoInfo
import Views.Helpers.GetAutoInfo
import Views.Helpers.ChooseOsagoEditStep
import Views.Helpers.ConfirmIdentity

inputOsagoData :: UserInfo -> Int -> Bool -> String -> IO UserInfo
inputOsagoData osagoUserInfo editStep False _ = do
  callCommand "cls" 

  (age, birthDate) <- if editStep == 1
    then inputDayOfBirth 16 100 
    else maybe (inputDayOfBirth 16 100) return (Views.UserInfo.birthDate osagoUserInfo)

  let infoMessage1 = "Выбран вид страхования: ОСАГО\nДата рождения: " ++ birthDate ++ "\nВозраст: " ++ show age

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

  let infoMessage4 = infoMessage3 ++ "\nРегион: " ++ (Entities.Regions.name region)

  putStrLn infoMessage4
  territorie <- if editStep == 3
    then chooseTerritorie (Entities.Regions.uid region) infoMessage4 
    else maybe (chooseTerritorie (Entities.Regions.uid region) infoMessage4) return (territorie osagoUserInfo)

  let infoMessage5 = infoMessage4 ++ "\nМесто проживания: " ++ (Entities.Territories.name territorie)

  typeKs <- if editStep == 4
    then сhooseTypeKS infoMessage5 
    else maybe (сhooseTypeKS infoMessage5) return (typeKS osagoUserInfo)

  let infoMessage6 = infoMessage5 ++ "\nСрок страхования: " ++ (Entities.TypeKS.description typeKs)

  typeKo <- if editStep == 5
    then сhooseTypeKO infoMessage6 
    else maybe (сhooseTypeKO infoMessage6) return (typeKO osagoUserInfo)

  let infoMessage7 = infoMessage6 ++ "\nКоличество водителей: " ++ (Entities.TypeKO.description typeKo)

  callCommand "cls" 
  editPunkt <- chooseOsagoEditStep False infoMessage7
  
  let osagoInfo = UserInfo {Views.UserInfo.birthDate = Just (age, birthDate), 
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

  case certificate of 
    Nothing -> return nullUserInfo
    Just cert -> do 

        activeOsago <- getActivePolicy (Entities.TransportCertificate.uid cert) 0

        case activeOsago of
          Nothing -> do 
              driver <- getDriverById (Entities.TransportCertificate.driverId cert) 

              isSusscessfulIdentification <- if editStep == 1 || editStep == -1
                then confirmIdentity driver
                else return True

              if not (isSusscessfulIdentification) then return nullUserInfo
              else do 
                age <- calcAgeFromDate (Entities.Drivers.birthday driver)

                let infoMessage1 = "\nВыбран вид страхования для оформления: ОСАГО" ++
                                  getAutoInfo enginePower transportBrand transportModel transport category (Just cert)

                region <- if editStep == 2
                then chooseRegion infoMessage1 
                else maybe (chooseRegion infoMessage1)  return (region osagoUserInfo)

                let infoMessage2 = infoMessage1 ++ "\nРегион: " ++ (Entities.Regions.name region)

                territorie <- if editStep == 2
                  then chooseTerritorie (Entities.Regions.uid region) infoMessage2 
                  else maybe (chooseTerritorie (Entities.Regions.uid region) infoMessage2) return (territorie osagoUserInfo)

                let infoMessage3 = infoMessage2 ++ "\nМесто проживания: " ++ (Entities.Territories.name territorie)

                typeKs <- if editStep == 3
                then сhooseTypeKS infoMessage3 
                else maybe (сhooseTypeKS infoMessage3) return (typeKS osagoUserInfo)

                let infoMessage4 = infoMessage3 ++ "\nСрок страхования: " ++ (Entities.TypeKS.description typeKs)

                typeKo <- if editStep == 4
                then сhooseTypeKO infoMessage4 
                else maybe (сhooseTypeKO infoMessage4) return (typeKO osagoUserInfo)

                let infoMessage5 = infoMessage4 ++ "\nКоличество водителей: " ++ (Entities.TypeKO.description typeKo)

                let osagoInfo = UserInfo {Views.UserInfo.birthDate = Just (age, (Entities.Drivers.birthday driver)),
                    drivingExpirience = Just (Entities.Drivers.experience driver),
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
              inputOsagoData (osagoUserInfo { Views.UserInfo.birthDate = Nothing, 
                                  Views.UserInfo.drivingExpirience = Nothing,
                                  Views.UserInfo.autoInfo = Nothing }) (-1) True "На данный автомобиль уже зарегестрирован полис ОСАГО"

  

 
  
