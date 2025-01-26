module Modules.ChooseTransportBrand (chooseTransportBrand) where

import Enteties.TransportBrands
import Shared.Inputs.ChooseData (chooseApiPaginatedData)

chooseTransportBrand :: String ->  IO (TransportBrand)
chooseTransportBrand infoMessage = chooseApiPaginatedData 0 "" 20 getTransportBrands 
                                    name "\nВыберите модель автомобиля" infoMessage

