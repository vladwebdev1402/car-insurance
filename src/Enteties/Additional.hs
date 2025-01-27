module Enteties.Additional (Additional(..), getAdditionals) where

import Shared.Api.GetAllData

data Additional = Additional { uid :: Int, value :: Float } deriving (Read, Show)

getAdditionals :: IO [Additional]
getAdditionals = getAllData "database/Additional.hdb"