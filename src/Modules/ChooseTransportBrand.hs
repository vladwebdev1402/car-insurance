module Modules.ChooseTransportBrand (chooseTransportBrand) where

import Enteties.TransportBrands
import Shared.Inputs.ChooseData (chooseApiPaginatedData)

chooseTransportBrand :: IO (TransportBrand)
chooseTransportBrand = chooseApiPaginatedData 0 "" getTransportBrands name

