module Enteties.Additional (Additional(..), getAdditional) where

import Shared.Api.GetAllData

data Additional = Additional { uid :: Int, value :: Float } deriving (Read, Show)

getAdditional :: IO [Additional]
getAdditional = getAllData "database/Additional.hdb"