module Modules.ChooseRegion (chooseRegion) where

import Enteties.Regions
import Shared.Inputs.ChooseData (chooseApiPaginatedData)

chooseRegion :: String -> IO (Region)
chooseRegion infoMessage = chooseApiPaginatedData 0 "" getRegions name "\nВыберите регион проживания:" infoMessage