module Modules.ChooseTypeTransport (chooseTypeTransport) where

import Enteties.TypesTransport
import Shared.Inputs.ChooseData (chooseData)
import Shared.Logs.LogData (generateLogData)

chooseTypeTransport :: IO (TypeTransport)
chooseTypeTransport = do
  typesTransport <- getTypesTransport
  putStrLn "Выберите категорию автомобиля:"
  index <- chooseData typesTransport (\array -> generateLogData array Enteties.TypesTransport.description)
  return $ typesTransport !! (index - 1)