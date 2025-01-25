module Enteties.Deductibles (Deductible(..), getDeductibles) where

import Shared.Api.GetAllData

data Deductible = Deductible {uid :: Int, sumDeductible :: Float} deriving (Read, Show)

getDeductibles :: IO [Deductible]
getDeductibles = getAllData "database/Deductibles.hdb"
