module Modules.ChooseTransport (chooseTransport) where

import Enteties.Transports
import Shared.Inputs.ChooseData (chooseData)
import Shared.Logs.LogData (generateLogData)

chooseTransport :: Int -> String -> IO (Transport)
chooseTransport modelId infoMessage = do
    transports <- getTransportByModelId 0 100 "" modelId
    index <- chooseData transports (\array -> generateLogData array (\x -> (show (year x)))) "\nВыберите год выпуска:" infoMessage
    return $ transports !! (index - 1)
    

