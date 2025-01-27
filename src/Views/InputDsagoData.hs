module Views.InputDsagoData (inputDsagoData) where

import System.Process (callCommand)
import Text.Printf (printf)
import Enteties.Regions
import Enteties.Territories
import Enteties.TypeKS
import Enteties.TypeKO
import Enteties.TransportBrands 
import Enteties.TransportModels 
import Enteties.Transports
import Enteties.TypesTransport
import Enteties.TransportCertificate
import Enteties.PolicyServices
import Enteties.Deductibles
import Enteties.Drivers
import Enteties.Policies
import Enteties.Additional
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
  callCommand "cls" 
  (age, birthDate) <- if editStep == 1
    then inputDayOfBirth 16 100 
    else maybe (inputDayOfBirth 16 100) return (Views.UserInfo.birthDate dsagoUserInfo)

  let infoMessage1 = "Выбран вид страхования для расчёта: КАСКО\nДата рождения: " ++ birthDate ++ "\nВозраст: " ++ show age
    
  callCommand "cls" 
  drivingExpirience <- if editStep == 1
    then inputRangeNumber infoMessage1 "Введите стаж вождения: " 0 (getMaximumDrivingExpirience age) 
    else maybe (inputRangeNumber infoMessage1 "Введите стаж вождения: " 0 (getMaximumDrivingExpirience age)) return (drivingExpirience dsagoUserInfo)

  let infoMessage2 = infoMessage1 ++ "\nСтаж вождения: " ++ show drivingExpirience

  callCommand "cls" 
  (enginePower, transportBrand, transportModel, transport, category, _) <- if editStep == 2
    then inputAutoInfo False infoMessage2 
    else maybe (inputAutoInfo False infoMessage2) return (autoInfo dsagoUserInfo)

  let infoMessage3 = infoMessage2 ++ (getAutoInfo enginePower transportBrand transportModel transport category Nothing)

  callCommand "cls" 
  region <- if editStep == 3
    then chooseRegion infoMessage3 
    else maybe (chooseRegion infoMessage3)  return (region dsagoUserInfo)

  let infoMessage4 = infoMessage3 ++ "\nРегион: " ++ (Enteties.Regions.name region)

  callCommand "cls" 
  territorie <- if editStep == 3
    then chooseTerritorie (Enteties.Regions.uid region) infoMessage4 
    else maybe (chooseTerritorie (Enteties.Regions.uid region) infoMessage4) return (territorie dsagoUserInfo)

  let infoMessage5 = infoMessage4 ++ "\nМесто проживания: " ++ (Enteties.Territories.name territorie)

  callCommand "cls" 
  typeKs <- if editStep == 4
    then сhooseTypeKS infoMessage5 
    else maybe (сhooseTypeKS infoMessage5) return (typeKS dsagoUserInfo)

  let infoMessage6 = infoMessage5 ++ "\nСрок страхования: " ++ (Enteties.TypeKS.description typeKs)

  callCommand "cls" 
  typeKo <- if editStep == 5
    then сhooseTypeKO infoMessage6 
    else maybe (сhooseTypeKO infoMessage6) return (typeKO dsagoUserInfo)

  let infoMessage7 = infoMessage6 ++ "\nКоличество водителей: " ++ (Enteties.TypeKO.description typeKo)

  callCommand "cls" 
  additional <- if editStep == 6
    then chooseAdditional infoMessage7 
    else maybe (chooseAdditional infoMessage7) return (Views.UserInfo.additional dsagoUserInfo)

  let infoMessage8 = infoMessage7 ++ "\nДополнительная сумма: " ++ (show (Enteties.Additional.value additional))
  callCommand "cls" 
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

    