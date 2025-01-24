module Shared.Validators.IsNothing (isNothing) where

isNothing :: Maybe a -> Bool
isNothing Nothing = False
isNothing _ = True