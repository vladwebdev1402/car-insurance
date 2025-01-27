module Modules.ChooseRegion (chooseRegion) where

import Entities.Regions
import Shared.Inputs.ChooseData (chooseApiPaginatedData)

chooseRegion :: String -> IO (Region)
chooseRegion infoMessage = chooseApiPaginatedData 0 "" 20 getRegions name "\nВыберите регион проживания:" infoMessage