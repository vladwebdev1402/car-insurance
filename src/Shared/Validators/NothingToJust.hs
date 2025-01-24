module Shared.Validators.NothingToJust (nothingToJust) where

nothingToJust :: Maybe a -> String -> a
nothingToJust Nothing errorMessage = error errorMessage
nothingToJust (Just x) _ = x