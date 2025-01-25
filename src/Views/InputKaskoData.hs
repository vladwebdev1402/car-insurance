module Views.InputKaskoData (KaskoUserInfo(..), inputKaskoData, nullKaskoUserInfo) where

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
import Modules.ChooseRegion
import Modules.ChooseTerritorie
import Modules.ChooseTypeKS
import Modules.ChooseTypeKO
import Modules.ChooseDeducatble
import Modules.ChoosePolicyServices
import Shared.Inputs.InputDayOfBirth
import Shared.Inputs.InputRangeNumber
import Shared.Calc.GetMaximumDrivingExpirience
import Views.Helpers.InputAutoInfo
import Views.Helpers.GetAutoInfo
import Views.Helpers.ChooseKaskoEditStep

data KaskoUserInfo = KaskoUserInfo {
  birthDate :: Maybe (Int, String),
  drivingExpirience :: Maybe Int,
  autoInfo :: Maybe (Int, Maybe TransportBrand, Maybe TransportModel, Maybe Transport, TypeTransport, Maybe TransportCertificate),
  region :: Maybe Region,
  territorie :: Maybe Territorie,
  typeKS :: Maybe TypeKS,
  typeKO :: Maybe TypeKO,
  deductible :: Maybe Deductible,
  policyServices :: Maybe [PolicyService]  
  }

nullKaskoUserInfo :: KaskoUserInfo
nullKaskoUserInfo = KaskoUserInfo {Views.InputKaskoData.birthDate = Nothing, 
                             Views.InputKaskoData.drivingExpirience = Nothing,
                             Views.InputKaskoData.autoInfo = Nothing, 
                             Views.InputKaskoData.region = Nothing, 
                             Views.InputKaskoData.territorie = Nothing, 
                             Views.InputKaskoData.typeKS = Nothing, 
                             Views.InputKaskoData.typeKO = Nothing,
                             Views.InputKaskoData.deductible = Nothing,
                             Views.InputKaskoData.policyServices = Nothing
                             }

inputKaskoData :: KaskoUserInfo -> Int -> Bool -> String -> IO KaskoUserInfo
inputKaskoData kaskoUserInfo editStep False _ = do
  callCommand "cls" 
  (age, birthDate) <- if editStep == 1
    then inputDayOfBirth 16 100 
    else maybe (inputDayOfBirth 16 100) return (Views.InputKaskoData.birthDate kaskoUserInfo)

  let infoMessage1 = "Выбран тип страховки для расчёта: КАСКО\nДата рождения: " ++ birthDate ++ "\nВозраст: " ++ show age

    
  callCommand "cls" 
  drivingExpirience <- if editStep == 1
    then inputRangeNumber infoMessage1 "Введите стаж вождения: " 0 (getMaximumDrivingExpirience age) 
    else maybe (inputRangeNumber infoMessage1 "Введите стаж вождения: " 0 (getMaximumDrivingExpirience age)) return (drivingExpirience kaskoUserInfo)

  let infoMessage2 = infoMessage1 ++ "\nСтаж вождения: " ++ show drivingExpirience

  callCommand "cls" 
  (enginePower, transportBrand, transportModel, transport, category, _) <- if editStep == 2
    then inputAutoInfo False infoMessage2 
    else maybe (inputAutoInfo False infoMessage2) return (autoInfo kaskoUserInfo)

  let infoMessage3 = infoMessage2 ++ (getAutoInfo enginePower transportBrand transportModel transport category Nothing)

  callCommand "cls" 
  region <- if editStep == 3
    then chooseRegion infoMessage3 
    else maybe (chooseRegion infoMessage3)  return (region kaskoUserInfo)

  let infoMessage4 = infoMessage3 ++ "\nРегион: " ++ (Enteties.Regions.name region)

  callCommand "cls" 
  territorie <- if editStep == 3
    then chooseTerritorie (Enteties.Regions.uid region) infoMessage4 
    else maybe (chooseTerritorie (Enteties.Regions.uid region) infoMessage4) return (territorie kaskoUserInfo)

  let infoMessage5 = infoMessage4 ++ "\nМесто проживания: " ++ (Enteties.Territories.name territorie)

  callCommand "cls" 
  typeKs <- if editStep == 4
    then сhooseTypeKS infoMessage5 
    else maybe (сhooseTypeKS infoMessage5) return (typeKS kaskoUserInfo)

  let infoMessage6 = infoMessage5 ++ "\nСрок страхования: " ++ (Enteties.TypeKS.description typeKs)

  callCommand "cls" 
  typeKo <- if editStep == 5
    then сhooseTypeKO infoMessage6 
    else maybe (сhooseTypeKO infoMessage6) return (typeKO kaskoUserInfo)

  let infoMessage7 = infoMessage6 ++ "\nКоличество водителей: " ++ (Enteties.TypeKO.description typeKo)

  callCommand "cls" 
  deductible <- if editStep == 6
    then chooseDeducatble infoMessage7 
    else maybe (chooseDeducatble infoMessage7) return (deductible kaskoUserInfo)

  let infoMessage8 = infoMessage7 ++ "\nФраншиза: " ++ (printf "%f" (Enteties.Deductibles.sumDeductible deductible))

  callCommand "cls" 
  services <- if editStep == 7
    then choosePolicyServices infoMessage7 
    else maybe (choosePolicyServices infoMessage7) return (policyServices kaskoUserInfo) 

  let infoMessage9 = infoMessage8 ++ "\nВыбранные услуги: " ++ (concat (map (\x -> (Enteties.PolicyServices.name x) ++ ", ") services))

  callCommand "cls" 
  editPunkt <- chooseKaskoEditStep False infoMessage9
  
  let kaskoInfo = KaskoUserInfo {Views.InputKaskoData.birthDate = Just (age, birthDate), 
          drivingExpirience = Just drivingExpirience,
          autoInfo = Just (enginePower, transportBrand, transportModel, transport, category, Nothing), 
          region = Just region, 
          territorie = Just territorie, 
          typeKS = Just typeKs, 
          typeKO = Just typeKo,
          deductible = Just deductible,
          policyServices = Just services
          }

  case editPunkt of 
    (-1) -> return kaskoInfo
    _ -> inputKaskoData kaskoInfo editPunkt False ""