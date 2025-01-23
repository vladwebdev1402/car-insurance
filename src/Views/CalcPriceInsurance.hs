module Views.CalcPriceInsurance (calcPriceInsurance) where

import System.Process (callCommand)
import System.IO
import Enteties.PolicyTypes 
import Enteties.TransportBrands 
import Enteties.TransportModels 
import Enteties.Transports
import Shared.Logs.LogData
import Shared.Validators.ValidateNumberRangeInput
import Shared.Inputs.ChooseData
import Shared.Inputs.InputRangeNumber
import Shared.Calc.GetMaximumDrivingExpirience
import Modules.ChooseTransportBrand
import Modules.ChooseTransportModel
import Modules.ChooseTransport

calcPriceInsurance :: IO ()
calcPriceInsurance = do
  policyTypes <- getPolicyTypes
  putStrLn "Выберите тип страховки:"
  index <- chooseData policyTypes (\array -> generateLogData array Enteties.PolicyTypes.name)
  let policyType = policyTypes !! (index - 1)

  case (Enteties.PolicyTypes.uid policyType) of
    0 -> calcOsagoPrice
    1 -> calcKaskoPrice
    _ -> return ()

calcOsagoPrice :: IO ()
calcOsagoPrice = do
  callCommand "cls" 
  age <- inputRangeNumber "Выбран тип страховки: ОСАГО" "Введите возраст: " 16 100
  callCommand "cls" 
  drivingExpirience <- inputRangeNumber ("Выбран тип страховки: ОСАГО\nВведённый возраст: " ++ show age) "Введите стаж вождения: " 0 (getMaximumDrivingExpirience age)
  callCommand "cls" 
  putStrLn $ "Выбран тип страховки: ОСАГО\nВведённый возраст: " ++ show age ++ "\nВведённый стаж вождения: " ++ show drivingExpirience
  autoInfoMode <- inputRangeNumber "Выберите режим ввода данных для информации об автомобиле:\n1. Ввод с клавиатуры\n2. Поиск автомобиля из базы данных" "" 1 2

  case autoInfoMode of
    1 -> do
        callCommand "cls" 
        enginePower <- inputRangeNumber "Введите мощность двигателя: " "" 16 450
        putStrLn $ "Выбран тип страховки: ОСАГО\n\
           \Введённый возраст: " ++ show age ++ "\n\
           \Введённый стаж вождения: " ++ show drivingExpirience ++ "\n\
           \Мощность двигателя: " ++ show enginePower
           
    2 -> do 
        callCommand "cls"
        transportBrand <- chooseTransportBrand
        putStrLn $ (Enteties.TransportBrands.name transportBrand)
        transportModel <- chooseTransportModel (Enteties.TransportBrands.uid transportBrand)
        putStrLn $ (Enteties.TransportModels.name transportModel)
        transport <- chooseTransport (Enteties.TransportModels.uid transportModel)
        putStrLn $ (show (Enteties.Transports.year transport))


  
calcKaskoPrice :: IO ()
calcKaskoPrice = do
  putStrLn $ "Выбран тип страховки: КАСКО"
