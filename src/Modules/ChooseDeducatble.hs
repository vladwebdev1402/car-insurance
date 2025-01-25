module Modules.ChooseDeducatble (chooseDeducatble) where

import Enteties.Deductibles
import Shared.Inputs.ChooseData (chooseData)
import Shared.Logs.LogData (generateLogData)

chooseDeducatble :: String -> IO (Deductible)
chooseDeducatble infoMessage = do
  deductibles <- getDeductibles
  index <- chooseData deductibles (\array -> generateLogData array (\item -> (show (Enteties.Deductibles.sumDeductible item)))) "\nВыберите размер франшизы:" infoMessage
  return $ deductibles !! (index - 1)