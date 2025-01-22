module Modules.ChooseTransportBrand (chooseTransportBrand) where

import System.Process (callCommand)
import Enteties.TransportBrands
import Shared.Inputs.ChooseData (chooseApiPaginatedData)
import Shared.Logs.LogData

chooseTransportBrand :: IO (TransportBrand)
chooseTransportBrand = chooseApiPaginatedData 0 "" getTransportBrands name

