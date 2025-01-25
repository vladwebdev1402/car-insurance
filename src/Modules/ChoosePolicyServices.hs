module Modules.ChoosePolicyServices (choosePolicyServices) where

import Enteties.PolicyServices
import Shared.Inputs.ChooseData (multyplyChooseData)
import Shared.Logs.LogData (generateLogData)

choosePolicyServices :: String -> IO [PolicyService]
choosePolicyServices infoMessage = do
  services <- getPolicyServices
  indexes <- multyplyChooseData services (\array -> generateLogData array name) "\nВыберите услуги. Введите числа через пробел:" infoMessage
  return (map (\x -> services !! (x - 1)) indexes)