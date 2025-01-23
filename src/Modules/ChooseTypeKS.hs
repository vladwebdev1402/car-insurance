module Modules.ChooseTypeKS (сhooseTypeKS) where

import Enteties.TypeKS
import Shared.Inputs.ChooseData (chooseData)
import Shared.Logs.LogData (generateLogData)

сhooseTypeKS :: String ->  IO (TypeKS)
сhooseTypeKS infoMessage = do
  typesKS <- getTypeKS
  index <- chooseData typesKS (\array -> generateLogData array Enteties.TypeKS.description) "\nВыберите срок страхования:" infoMessage
  return $ typesKS !! (index - 1)