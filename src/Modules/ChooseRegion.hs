module Modules.ChooseRegion (chooseRegion) where

import Enteties.Regions
import Shared.Inputs.ChooseData (chooseApiPaginatedData)

chooseRegion :: IO (Region)
chooseRegion = chooseApiPaginatedData 0 "" getRegions name