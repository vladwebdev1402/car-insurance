module Shared.Validators.ValidateStringNumber (validateStringNumber) where

import Text.Read (readMaybe)
import Shared.Validators.IsNumber

validateStringNumber :: String -> Int -> Int -> Bool
validateStringNumber input minVal maxVal =
    case readMaybe input of
        Just num -> isNumber input && num >= minVal && num <= maxVal
        Nothing  -> False
