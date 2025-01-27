module Modules.ChoosePolicyType (choosePolicyType) where

import Entities.PolicyTypes
import Shared.Inputs.ChooseData (chooseData)
import Shared.Logs.LogData (generateLogData)

choosePolicyType :: IO (PolicyType)
choosePolicyType = do
  policyTypes <- getPolicyTypes
  index <- chooseData policyTypes (\array -> generateLogData array Entities.PolicyTypes.name) "Введите 'выход', чтобы вернуться в меню\nВыберите вид страхования:" ""
  case index of
    -1 -> return PolicyType {uid = -1, name = ""}
    _ -> return $ policyTypes !! (index - 1)