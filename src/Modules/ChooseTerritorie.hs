module Modules.ChooseTerritorie (chooseTerritorie) where

import Enteties.Territories
import Shared.Inputs.ChooseData (chooseApiParamPaginatedData)

chooseTerritorie :: Int -> String -> IO (Territorie)
chooseTerritorie region infoMessage = chooseApiParamPaginatedData 0 "" region getTerritoriesByRegionId 
                name "\nВыберите место проживания:" infoMessage