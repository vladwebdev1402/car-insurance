module Modules.ChooseTerritorie (chooseTerritorie) where

import Enteties.Territories
import Shared.Inputs.ChooseData (chooseApiParamPaginatedData)

chooseTerritorie :: Int ->  IO (Territorie)
chooseTerritorie region = chooseApiParamPaginatedData 0 "" region getTerritoriesByRegionId name