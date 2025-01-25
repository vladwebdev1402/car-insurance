module Views.CalcPriceInsurance (calcPriceInsurance) where

import Text.Printf (printf)
import Views.InputOsagoData
import Views.Helpers.ChooseOsagoEditStep
import Views.CalcOsagoPrices
import Enteties.PolicyTypes
import Enteties.Companys
import Modules.ChoosePolicyType
import Shared.Logs.LogData

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
        Nothing -> nullOsagoUserInfo
        _ -> osagoUserInfo

  osagoUserInfo <- inputOsagoData osagoUserInfo editPunkt False "" 

  companysWithPrice <- calcOsagoPrices osagoUserInfo

  let infoMessage = generateLogString companysWithPrice (\(company, price) -> (Enteties.Companys.name company) ++ " - " ++ (printf "%.2f" price))

  updatePunkt <- chooseOsagoEditStep False infoMessage
 
  case updatePunkt of 
    -1 -> return ()
    _ -> calcOsagoPrice (Just osagoUserInfo) updatePunkt

calcKaskoPrice :: IO ()
calcKaskoPrice = do
  putStrLn $ "Выбран тип страховки: КАСКО"

  
