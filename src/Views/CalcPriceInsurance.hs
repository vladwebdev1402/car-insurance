module Views.CalcPriceInsurance (calcPriceInsurance) where

import System.Process (callCommand)
import System.IO
import Enteties.PolicyTypes 
import Enteties.TransportBrands 
import Enteties.TransportModels 
import Enteties.Transports
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
import Modules.ChooseRegion
import Modules.ChooseTerritorie
import Modules.ChoosePolicyType
import Modules.ChooseTypeKS
import Modules.ChooseTypeKO

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
  callCommand "cls" 
  drivingExpirience <- inputRangeNumber ("Выбран тип страховки: ОСАГО\nВведённый возраст: " ++ show age) "Введите стаж вождения: " 0 (getMaximumDrivingExpirience age)
  callCommand "cls" 
  putStrLn $ "Выбран тип страховки: ОСАГО\nВведённый возраст: " ++ show age ++ "\nВведённый стаж вождения: " ++ show drivingExpirience
  autoInfoMode <- inputRangeNumber "Выберите режим ввода данных для информации об автомобиле:\n1. Ввод с клавиатуры\n2. Поиск автомобиля из базы данных" "" 1 2
  
  enginePower <- case autoInfoMode of
    1 -> do
        callCommand "cls" 
        inputRangeNumber "Введите мощность двигателя: " "" 16 450
    2 -> do 
        callCommand "cls"
        transportBrand <- chooseTransportBrand
        putStrLn $ (Enteties.TransportBrands.name transportBrand)
        transportModel <- chooseTransportModel (Enteties.TransportBrands.uid transportBrand)
        putStrLn $ (Enteties.TransportModels.name transportModel)
        transport <- chooseTransport (Enteties.TransportModels.uid transportModel)
        putStrLn $ (show (Enteties.Transports.year transport))
        return (Enteties.Transports.power transport)
  
  putStrLn $ "Выбран тип страховки: ОСАГО\nВведённый возраст: " ++ show age ++ "\nВведённый стаж вождения: " ++ show drivingExpirience ++ "\nВведённая мощность двигателя: " ++ show enginePower
  
  region <- chooseRegion
  putStrLn $ (Enteties.Regions.name region)
  territorie <- chooseTerritorie (Enteties.Regions.uid region)
  putStrLn $ (Enteties.Territories.name territorie)
  typeKs <- сhooseTypeKS
  putStrLn $ (show (Enteties.TypeKS.countMonths typeKs))
  typeKo <- сhooseTypeKO
  putStrLn $ (Enteties.TypeKO.description typeKo)
  
calcKaskoPrice :: IO ()
calcKaskoPrice = do
  putStrLn $ "Выбран тип страховки: КАСКО"
