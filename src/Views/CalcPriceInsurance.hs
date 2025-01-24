module Views.CalcPriceInsurance (calcPriceInsurance) where

import Text.Printf (printf)
import Data.List
import Views.InputOsagoData
import Views.Helpers.ChooseOsagoEditStep
import Views.CalcOsagoPrices
import Enteties.PolicyTypes
import Enteties.TypesKVS
import Enteties.TypesKM
import Enteties.TypesKBM
import Enteties.TypeKS
import Enteties.TypeKO
import Enteties.CoefTB
import Enteties.Companys
import Enteties.TypesTransport
import Enteties.CompanyPolicyLink
import Enteties.Territories
import Modules.ChoosePolicyType
import Shared.Logs.LogData
import Shared.Inputs.ChooseData
import Views.Helpers.InputUserPassport

import System.Process (callCommand)


calcPriceInsurance :: IO ()
calcPriceInsurance = do
  policyType <- choosePolicyType

  case (Enteties.PolicyTypes.uid policyType) of
    0 -> calcOsagoPrice Nothing (-1)
    1 -> calcKaskoPrice
    _ -> return ()

calcOsagoPrice :: Maybe OsagoUserInfo -> Int -> IO ()
calcOsagoPrice oldOsagoUserInfo editPunkt = do

  let osagoUserInfo = case oldOsagoUserInfo of
        Nothing -> OsagoUserInfo {birthDate = Nothing, passport = Nothing, drivingExpirience = Nothing,
          autoInfo = Nothing, region = Nothing, territorie = Nothing, typeKS = Nothing, typeKO = Nothing}
        Just osagoUserInfo -> osagoUserInfo

  osagoUserInfo <- inputOsagoData osagoUserInfo editPunkt False   

  companysWithPrice <- calcOsagoPrices osagoUserInfo

  let infoMessage = generateLogString companysWithPrice (\(company, price) -> (Enteties.Companys.name company) ++ " - " ++ (printf "%.2f" price))

  updatePunkt <- chooseOsagoEditStep infoMessage
 
  case updatePunkt of 
    -1 -> putStrLn "Конец расчёт"
    _ -> calcOsagoPrice (Just osagoUserInfo) updatePunkt

calcKaskoPrice :: IO ()
calcKaskoPrice = do
  putStrLn $ "Выбран тип страховки: КАСКО"

  
