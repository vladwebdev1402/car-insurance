module Shared.Validators.IsNumber (isNumber) where

isNumber :: String -> Bool
isNumber = all (`elem` "0123456789")