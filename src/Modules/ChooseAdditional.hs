module Modules.ChooseAdditional (chooseAdditional) where

import Entities.Additional
import Shared.Inputs.ChooseData (chooseData)
import Shared.Logs.LogData (generateLogData)
import Text.Printf (printf)
import Shared.Logs.FormateNumber

chooseAdditional :: String -> IO (Additional)
chooseAdditional infoMessage = do
  additionals <- getAdditionals
  index <- chooseData additionals (\array -> generateLogData array (\item -> (formateNumber (printf "%.2f" (Entities.Additional.value item))))) "\nВыберите размер дополнительной суммы:" infoMessage
  return $ additionals !! (index - 1)