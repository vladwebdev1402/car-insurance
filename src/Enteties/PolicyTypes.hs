module Enteties.PolicyTypes (PolicyType(..), getPolicyTypes) where

import System.IO
import GHC.IO.Encoding (setLocaleEncoding)
import Shared.Api.GetAllData

data PolicyType = PolicyType { uid :: Int, name :: String } deriving (Read, Show)

getPolicyTypes :: IO [PolicyType]
getPolicyTypes = getAllData "database/PolicyTypes.hdb"