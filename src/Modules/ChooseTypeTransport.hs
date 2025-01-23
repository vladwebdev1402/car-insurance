module Modules.ChooseTypeTransport (chooseTypeTransport) where

import Enteties.TypesTransport
import Shared.Inputs.ChooseData (chooseData)
import Shared.Logs.LogData (generateLogData)

chooseTypeTransport :: String -> IO (TypeTransport)
chooseTypeTransport infoMessage = do
  typesTransport <- getTypesTransport
  index <- chooseData typesTransport (\array -> generateLogData array Enteties.TypesTransport.description) "\nВыберите категорию автомобиля:" infoMessage
  return $ typesTransport !! (index - 1)