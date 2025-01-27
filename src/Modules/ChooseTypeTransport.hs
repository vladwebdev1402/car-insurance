module Modules.ChooseTypeTransport (chooseTypeTransport) where

import Entities.TypesTransport
import Shared.Inputs.ChooseData (chooseData)
import Shared.Logs.LogData (generateLogData)

chooseTypeTransport :: String -> IO (TypeTransport)
chooseTypeTransport infoMessage = do
  typesTransport <- getTypesTransport
  index <- chooseData typesTransport (\array -> generateLogData array Entities.TypesTransport.description) "\nВыберите категорию автомобиля:" infoMessage
  return $ typesTransport !! (index - 1)