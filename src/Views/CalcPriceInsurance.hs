module Views.CalcPriceInsurance (calcPriceInsurance) where

import Views.InputOsagoData 
import Enteties.PolicyTypes
import Modules.ChoosePolicyType

import System.Process (callCommand)

calcPriceInsurance :: IO ()
calcPriceInsurance = do
  policyType <- choosePolicyType

  case (Enteties.PolicyTypes.uid policyType) of
    0 -> calcOsagoPrice False
    1 -> calcKaskoPrice
    _ -> return ()

calcOsagoPrice :: Bool -> IO ()
calcOsagoPrice isRegistration = do
  osagoUserInfo <- inputOsagoData OsagoUserInfo {birthDate = Nothing, drivingExpirience = Nothing,
          autoInfo = Nothing, region = Nothing, territorie = Nothing, typeKS = Nothing, typeKO = Nothing} (-1) isRegistration

  (age, date) <- maybe (pure (0, "")) return (Views.InputOsagoData.birthDate osagoUserInfo)

  putStrLn date

calcKaskoPrice :: IO ()
calcKaskoPrice = do
  putStrLn $ "Выбран тип страховки: КАСКО"
