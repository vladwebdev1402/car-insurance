module Modules.ChoosePolicyType (choosePolicyType) where

import Enteties.PolicyTypes
import Shared.Inputs.ChooseData (chooseData)
import Shared.Logs.LogData (generateLogData)

choosePolicyType :: IO (PolicyType)
choosePolicyType = do
  policyTypes <- getPolicyTypes
  index <- chooseData policyTypes (\array -> generateLogData array Enteties.PolicyTypes.name) "\nВыберите вид страхования:" ""
  return $ policyTypes !! (index - 1)