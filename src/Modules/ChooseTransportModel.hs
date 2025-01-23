module Modules.ChooseTransportModel (chooseTransportModel) where

import Enteties.TransportModels 
import Shared.Inputs.ChooseData (chooseApiParamPaginatedData)

chooseTransportModel :: Int ->  IO (TransportModel)
chooseTransportModel brandId = chooseApiParamPaginatedData 0 "" brandId getTransportModelsByBrandId name
