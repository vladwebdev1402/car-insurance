module Modules.ChooseTypeKO (сhooseTypeKO) where

import Entities.TypeKO
import Shared.Inputs.ChooseData (chooseData)
import Shared.Logs.LogData (generateLogData)

сhooseTypeKO :: String -> IO (TypeKO)
сhooseTypeKO infoMessage = do
  typesKO <- getTypeKO
  index <- chooseData typesKO (\array -> generateLogData array Entities.TypeKO.description) "\nВыберите количество водителей:" infoMessage
  return $ typesKO !! (index - 1)