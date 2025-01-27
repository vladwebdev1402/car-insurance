module Modules.ChooseAdditional (chooseAdditional) where

import Entities.Additional
import Shared.Inputs.ChooseData (chooseData)
import Shared.Logs.LogData (generateLogData)

chooseAdditional :: String -> IO (Additional)
chooseAdditional infoMessage = do
  additionals <- getAdditionals
  index <- chooseData additionals (\array -> generateLogData array (\item -> (show (Entities.Additional.value item)))) "\nВыберите размер дополнительной суммы:" infoMessage
  return $ additionals !! (index - 1)