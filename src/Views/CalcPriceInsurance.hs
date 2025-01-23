module Views.CalcPriceInsurance (calcPriceInsurance) where

import System.Process (callCommand)
import Enteties.PolicyTypes 
import Enteties.Regions
import Enteties.Territories
import Enteties.TypeKS
import Enteties.TypeKO
import Shared.Inputs.InputRangeNumber
import Shared.Inputs.InputDayOfBirth
import Shared.Calc.GetMaximumDrivingExpirience
import Modules.ChooseRegion
import Modules.ChooseTerritorie
import Modules.ChoosePolicyType
import Modules.ChooseTypeKS
import Modules.ChooseTypeKO
import Views.Helpers.InputAutoInfo
import Views.Helpers.GetAutoInfo

calcPriceInsurance :: IO ()
calcPriceInsurance = do
  policyType <- choosePolicyType

  case (Enteties.PolicyTypes.uid policyType) of
    0 -> calcOsagoPrice
    1 -> calcKaskoPrice
    _ -> return ()

calcOsagoPrice :: IO ()
calcOsagoPrice = do

  callCommand "cls" 
  (age, birthDate) <- inputDayOfBirth 16 100
  let infoMessage1 = "Выбран тип страховки: ОСАГО\nДата рождения: " ++ birthDate ++ "\nВозраст: " ++ show age

  callCommand "cls" 
  drivingExpirience <- inputRangeNumber infoMessage1 "Введите стаж вождения: " 0 (getMaximumDrivingExpirience age)

  let infoMessage2 = infoMessage1 ++ "\nСтаж вождения: " ++ show drivingExpirience
  
  callCommand "cls" 
  (enginePower, transportBrand, transportModel, transport, category) <- inputAutoInfo False infoMessage2

  let infoMessage3 = infoMessage2 ++ (getAutoInfo enginePower transportBrand transportModel transport category)

  region <- chooseRegion infoMessage3
  let infoMessage4 = infoMessage3 ++ "\nРегион: " ++ (Enteties.Regions.name region)

  territorie <- chooseTerritorie (Enteties.Regions.uid region) infoMessage4
  let infoMessage5 = infoMessage4 ++ "\nМесто проживания: " ++ (Enteties.Territories.name territorie)

  typeKs <- сhooseTypeKS infoMessage5
  let infoMessage6 = infoMessage5 ++ "\nСрок страхования: " ++ (Enteties.TypeKS.description typeKs)

  typeKo <- сhooseTypeKO infoMessage6
  let infoMessage7 = infoMessage6 ++ "\nКоличество водителей: " ++ (Enteties.TypeKO.description typeKo)

  callCommand "cls" 
  putStrLn infoMessage7
  
calcKaskoPrice :: IO ()
calcKaskoPrice = do
  putStrLn $ "Выбран тип страховки: КАСКО"
