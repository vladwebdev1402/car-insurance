module Views.CalcPriceInsurance (calcPriceInsurance) where

import Text.Printf (printf)
import Views.InputOsagoData
import Views.InputDsagoData
import Views.InputKaskoData
import Views.UserInfo
import Views.Helpers.ChooseOsagoEditStep
import Views.Helpers.ChooseKaskoEditStep
import Views.Helpers.ChooseDsagoEditStep
import Views.CalcOsagoPrices
import Views.CalcKaskoPrices
import Views.CalcDsagoPrices
import Enteties.PolicyTypes
import Enteties.Companys
import Modules.ChoosePolicyType
import Shared.Logs.LogData

calcPriceInsurance :: IO ()
calcPriceInsurance = do
  policyType <- choosePolicyType

  case (Enteties.PolicyTypes.uid policyType) of
    0 -> calcOsagoPrice Nothing (-1)
    1 -> calcKaskoPrice Nothing (-1)
    2 -> calcDsagoPrice Nothing (-1)
    _ -> return ()

calcOsagoPrice :: Maybe UserInfo -> Int -> IO ()
calcOsagoPrice oldOsagoUserInfo editPunkt = do

  let osagoUserInfo = case oldOsagoUserInfo of
        Nothing -> nullUserInfo
        Just osagoInfo -> osagoInfo

  osagoUserInfo <- inputOsagoData osagoUserInfo editPunkt False "" 

  companysWithPrice <- calcOsagoPrices osagoUserInfo

  let infoMessage = generateLogString companysWithPrice (\(company, price) -> (Enteties.Companys.name company) ++ " - " ++ (printf "%.2f" price))

  updatePunkt <- chooseOsagoEditStep False infoMessage
 
  case updatePunkt of 
    -1 -> return ()
    _ -> calcOsagoPrice (Just osagoUserInfo) updatePunkt

calcKaskoPrice :: Maybe UserInfo -> Int -> IO ()
calcKaskoPrice oldKaskoUserInfo editPunkt = do
  let kaskoUserInfo = case oldKaskoUserInfo of
        Nothing -> nullUserInfo
        Just userInfo -> userInfo

  kaskoUserInfo <- inputKaskoData kaskoUserInfo editPunkt False "" 

  companysWithPrice <- calcKaskoPrices kaskoUserInfo

  let infoMessage = generateLogString companysWithPrice (\(company, price) -> (Enteties.Companys.name company) ++ " - " ++ (printf "%.2f" price))

  updatePunkt <- chooseKaskoEditStep False infoMessage

  case updatePunkt of 
    -1 -> return ()
    _ -> calcKaskoPrice (Just kaskoUserInfo) updatePunkt

calcDsagoPrice :: Maybe UserInfo -> Int -> IO ()
calcDsagoPrice oldDsagoUserInfo editPunkt = do
  let dsagoUserInfo = case oldDsagoUserInfo of
        Nothing -> nullUserInfo
        Just userInfo -> userInfo

  dsagoUserInfo <- inputDsagoData dsagoUserInfo editPunkt False "" 

  companysWithPrice <- calcDsagoPrices dsagoUserInfo

  let infoMessage = generateLogString companysWithPrice (\(company, price) -> (Enteties.Companys.name company) ++ " - " ++ (printf "%.2f" price))

  updatePunkt <- chooseDsagoEditStep False infoMessage

  case updatePunkt of 
    -1 -> return ()
    _ -> return ()

  

  
