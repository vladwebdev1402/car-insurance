module Views.CalcPriceInsurance (calcPriceInsurance) where

import System.Process (callCommand)
import System.IO
import Enteties.PolicyTypes 
import Enteties.TransportBrands 
import Enteties.TransportModels 
import Enteties.Transports
import Enteties.TypesTransport
import Enteties.Regions
import Enteties.Territories
import Enteties.TypeKS
import Enteties.TypeKO
import Shared.Logs.LogData
import Shared.Validators.ValidateNumberRangeInput
import Shared.Inputs.ChooseData
import Shared.Inputs.InputRangeNumber
import Shared.Inputs.InputDayOfBirth
import Shared.Calc.GetMaximumDrivingExpirience
import Modules.ChooseTransportBrand
import Modules.ChooseTransportModel
import Modules.ChooseTransport
import Modules.ChooseTypeTransport
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
  callCommand "cls" 
  let infoMessage2 = infoMessage1 ++ "\nСтаж вождения: " ++ show drivingExpirience
  putStrLn $ infoMessage2
  
  (enginePower, transportBrand, transportModel, transport, category) <- inputAutoInfo False

  let infoMessage3 = infoMessage2 ++ (getAutoInfo enginePower transportBrand transportModel transport category)

  putStrLn infoMessage3
  
  -- region <- chooseRegion
  -- putStrLn $ (Enteties.Regions.name region)
  -- territorie <- chooseTerritorie (Enteties.Regions.uid region)
  -- putStrLn $ (Enteties.Territories.name territorie)
  -- typeKs <- сhooseTypeKS
  -- putStrLn $ (show (Enteties.TypeKS.countMonths typeKs))
  -- typeKo <- сhooseTypeKO
  -- putStrLn $ (Enteties.TypeKO.description typeKo)
  
calcKaskoPrice :: IO ()
calcKaskoPrice = do
  putStrLn $ "Выбран тип страховки: КАСКО"
