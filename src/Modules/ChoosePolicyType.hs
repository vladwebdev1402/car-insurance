module Modules.ChoosePolicyType (choosePolicyType) where

import Entities.PolicyTypes
import Shared.Inputs.ChooseData (chooseData)
import Shared.Logs.LogData (generateLogData)

choosePolicyType :: IO (PolicyType)
choosePolicyType = do
  policyTypes <- getPolicyTypes
  index <- chooseData policyTypes (\array -> generateLogData array Entities.PolicyTypes.name) "\nВыберите вид страхования:" ""
  return $ policyTypes !! (index - 1)