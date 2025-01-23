module Modules.ChooseTypeKO (сhooseTypeKO) where

import Enteties.TypeKO
import Shared.Inputs.ChooseData (chooseData)
import Shared.Logs.LogData (generateLogData)

сhooseTypeKO :: IO (TypeKO)
сhooseTypeKO = do
  typesKO <- getTypeKO
  putStrLn "Выберите количество водителей:"
  index <- chooseData typesKO (\array -> generateLogData array Enteties.TypeKO.description)
  return $ typesKO !! (index - 1)