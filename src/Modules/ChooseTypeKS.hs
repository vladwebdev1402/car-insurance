module Modules.ChooseTypeKS (сhooseTypeKS) where

import Enteties.TypeKS
import Shared.Inputs.ChooseData (chooseData)
import Shared.Logs.LogData (generateLogData)

сhooseTypeKS :: IO (TypeKS)
сhooseTypeKS = do
  typesKS <- getTypeKS
  putStrLn "Выберите срок страхования:"
  index <- chooseData typesKS (\array -> generateLogData array Enteties.TypeKS.description)
  return $ typesKS !! (index - 1)