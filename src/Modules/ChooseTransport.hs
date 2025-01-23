module Modules.ChooseTransport (chooseTransport) where

import Enteties.Transports
import Shared.Inputs.ChooseData (chooseData)
import Shared.Logs.LogData (generateLogData)

chooseTransport :: Int -> IO (Transport)
chooseTransport modelId = do
    transports <- getTransportByModelId 0 100 "" modelId
    index <- chooseData transports (\array -> generateLogData array (\x -> (show (year x))))
    return $ transports !! (index - 1)
    

