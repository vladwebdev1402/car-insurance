module Modules.ChooseTransportModel (chooseTransportModel) where

import Enteties.TransportModels 
import Shared.Inputs.ChooseData (chooseApiParamPaginatedData)

chooseTransportModel :: Int -> String ->  IO (TransportModel)
chooseTransportModel brandId infoMessage = chooseApiParamPaginatedData 0 "" 20 brandId getTransportModelsByBrandId 
                    name "\nВыберите марку автомобиля" infoMessage
