module Views.InputDsagoData (inputDsagoData) where

import System.Process (callCommand)
import Text.Printf (printf)
import Entities.Regions
import Entities.Territories
import Entities.TypeKS
import Entities.TypeKO
import Entities.TransportBrands 
import Entities.TransportModels 
import Entities.Transports
import Entities.TypesTransport
import Entities.TransportCertificate
import Entities.PolicyServices
import Entities.Deductibles
import Entities.Drivers
import Entities.Policies
import Entities.Additional
import Modules.ChooseRegion
import Modules.ChooseTerritorie
import Modules.ChooseTypeKS
import Modules.ChooseTypeKO
import Modules.ChooseDeducatble
import Modules.ChoosePolicyServices
import Modules.ChooseAdditional
import Shared.Inputs.InputDayOfBirth
import Shared.Inputs.InputRangeNumber
import Shared.Validators.NothingToJust
import Shared.Calc.CalcAgeFromDate
import Shared.Logs.Console
import Shared.Calc.GetMaximumDrivingExpirience
import Views.UserInfo
import Views.Helpers.InputAutoInfo
import Views.Helpers.GetAutoInfo
import Views.Helpers.ChooseDsagoEditStep
import Views.Helpers.ConfirmIdentity

inputDsagoData :: UserInfo -> Int -> Bool -> String -> IO UserInfo
inputDsagoData dsagoUserInfo editStep False _ = do
  callCommand "clear" 
  (age, birthDate) <- if editStep == 1
    then inputDayOfBirth 16 100 
    else maybe (inputDayOfBirth 16 100) return (Views.UserInfo.birthDate dsagoUserInfo)

  let infoMessage1 = "Выбран вид страхования для расчёта: КАСКО\nДата рождения: " ++ birthDate ++ "\nВозраст: " ++ show age
    
  callCommand "clear" 
  drivingExpirience <- if editStep == 1
    then inputRangeNumber infoMessage1 "Введите стаж вождения: " 0 (getMaximumDrivingExpirience age) 
    else maybe (inputRangeNumber infoMessage1 "Введите стаж вождения: " 0 (getMaximumDrivingExpirience age)) return (drivingExpirience dsagoUserInfo)

  let infoMessage2 = infoMessage1 ++ "\nСтаж вождения: " ++ show drivingExpirience

  callCommand "clear" 
  (enginePower, transportBrand, transportModel, transport, category, _) <- if editStep == 2
    then inputAutoInfo False infoMessage2 
    else maybe (inputAutoInfo False infoMessage2) return (autoInfo dsagoUserInfo)

  let infoMessage3 = infoMessage2 ++ (getAutoInfo enginePower transportBrand transportModel transport category Nothing)

  callCommand "clear" 
  region <- if editStep == 3
    then chooseRegion infoMessage3 
    else maybe (chooseRegion infoMessage3)  return (region dsagoUserInfo)

  let infoMessage4 = infoMessage3 ++ "\nРегион: " ++ (Entities.Regions.name region)

  callCommand "clear" 
  territorie <- if editStep == 3
    then chooseTerritorie (Entities.Regions.uid region) infoMessage4 
    else maybe (chooseTerritorie (Entities.Regions.uid region) infoMessage4) return (territorie dsagoUserInfo)

  let infoMessage5 = infoMessage4 ++ "\nМесто проживания: " ++ (Entities.Territories.name territorie)

  callCommand "clear" 
  typeKs <- if editStep == 4
    then сhooseTypeKS infoMessage5 
    else maybe (сhooseTypeKS infoMessage5) return (typeKS dsagoUserInfo)

  let infoMessage6 = infoMessage5 ++ "\nСрок страхования: " ++ (Entities.TypeKS.description typeKs)

  callCommand "clear" 
  typeKo <- if editStep == 5
    then сhooseTypeKO infoMessage6 
    else maybe (сhooseTypeKO infoMessage6) return (typeKO dsagoUserInfo)

  let infoMessage7 = infoMessage6 ++ "\nКоличество водителей: " ++ (Entities.TypeKO.description typeKo)

  callCommand "clear" 
  additional <- if editStep == 6
    then chooseAdditional infoMessage7 
    else maybe (chooseAdditional infoMessage7) return (Views.UserInfo.additional dsagoUserInfo)

  let infoMessage8 = infoMessage7 ++ "\nДополнительная сумма: " ++ (show (Entities.Additional.value additional))
  callCommand "clear" 
  editPunkt <- chooseDsagoEditStep False infoMessage8
  
  let dsagoInfo = UserInfo {Views.UserInfo.birthDate = Just (age, birthDate), 
          drivingExpirience = Just drivingExpirience,
          autoInfo = Just (enginePower, transportBrand, transportModel, transport, category, Nothing), 
          region = Just region, 
          territorie = Just territorie, 
          typeKS = Just typeKs, 
          typeKO = Just typeKo,
          Views.UserInfo.additional = Just additional
          }

  case editPunkt of 
    (-1) -> return dsagoInfo
    _ -> inputDsagoData dsagoInfo editPunkt False ""

inputDsagoData dsagoUserInfo editStep True errorMessage = do
  callCommand "clear" 
  putStrLn errorMessage
  (enginePower, transportBrand, transportModel, transport, category, certificate) <- if editStep == 1
    then inputAutoInfo True errorMessage
    else maybe (inputAutoInfo True errorMessage) return (autoInfo dsagoUserInfo)

  case certificate of 
    Nothing -> return nullUserInfo
    Just cert -> do

      activeOsago <- getActivePolicy (Entities.TransportCertificate.uid cert) 0

      activeDsago <- getActivePolicy (Entities.TransportCertificate.uid cert) 2

      case activeOsago of 
        Nothing -> do
          callCommand "clear"
          inputDsagoData (dsagoUserInfo {Views.UserInfo.birthDate = Nothing, 
                                Views.UserInfo.drivingExpirience = Nothing,
                                Views.UserInfo.autoInfo = Nothing } ) (-1) True "У данного автомобиля нет активного ОСАГО полиса"
        _ -> case activeDsago of 
          Nothing -> do
            callCommand "clear"
            driver <- getDriverById (Entities.TransportCertificate.driverId cert) 

            isSusscessfulIdentification <- if editStep == 1 || editStep == -1
              then confirmIdentity driver
              else return True

            if not (isSusscessfulIdentification) then return nullUserInfo
            else do 
              age <- calcAgeFromDate (Entities.Drivers.birthday driver)

              let infoMessage1 = "\nВыбран вид страхования для оформления: ДСАГО" ++
                                getAutoInfo enginePower transportBrand transportModel transport category (Just cert)
              
              callCommand "clear" 
              region <- if editStep == 2
                then chooseRegion infoMessage1 
                else maybe (chooseRegion infoMessage1)  return (region dsagoUserInfo)

              let infoMessage2 = infoMessage1 ++ "\nРегион: " ++ (Entities.Regions.name region)

              callCommand "clear" 
              territorie <- if editStep == 2
                then chooseTerritorie (Entities.Regions.uid region) infoMessage2 
                else maybe (chooseTerritorie (Entities.Regions.uid region) infoMessage2) return (territorie dsagoUserInfo)

              let infoMessage3 = infoMessage2 ++ "\nМесто проживания: " ++ (Entities.Territories.name territorie)

              callCommand "clear" 
              typeKs <- if editStep == 3
                then сhooseTypeKS infoMessage3 
                else maybe (сhooseTypeKS infoMessage3) return (typeKS dsagoUserInfo)

              let infoMessage4 = infoMessage3 ++ "\nСрок страхования: " ++ (Entities.TypeKS.description typeKs)

              callCommand "clear" 
              typeKo <- if editStep == 3
                then сhooseTypeKO infoMessage4 
                else maybe (сhooseTypeKO infoMessage4) return (typeKO dsagoUserInfo)

              let infoMessage5 = infoMessage4 ++ "\nКоличество водителей: " ++ (Entities.TypeKO.description typeKo)

              callCommand "clear" 
              additional <- if editStep == 6
                then chooseAdditional infoMessage5 
                else maybe (chooseAdditional infoMessage5) return (Views.UserInfo.additional dsagoUserInfo)

              let infoMessage6 = infoMessage5 ++ "\nДополнительная сумма: " ++ (show (Entities.Additional.value additional))
              callCommand "clear" 
              editPunkt <- chooseDsagoEditStep False infoMessage6

              let dsagoInfo = UserInfo {Views.UserInfo.birthDate = Just (age, (Entities.Drivers.birthday driver)), 
                          drivingExpirience = Just (Entities.Drivers.experience driver),
                          autoInfo = Just (enginePower, transportBrand, transportModel, transport, category, (Just cert)), 
                          region = Just region, 
                          territorie = Just territorie, 
                          typeKS = Just typeKs, 
                          typeKO = Just typeKo,
                          Views.UserInfo.additional = Just additional
                      }

              case editPunkt of 
                (-1) -> return dsagoInfo
                _ -> inputDsagoData dsagoInfo editPunkt True ""

          _ -> do
              inputDsagoData (dsagoUserInfo {Views.UserInfo.birthDate = Nothing, 
                                Views.UserInfo.drivingExpirience = Nothing,
                                Views.UserInfo.autoInfo = Nothing } ) (-1) True "На данный автомобиль уже зарегестрирован полис ДСАГО"

    