module Shared.Validators.ValidateNumberRangeInput (validateNumberRangeInput) where

import Text.Read (readMaybe)

validateNumberRangeInput :: Int -> Int -> String -> Maybe Int
validateNumberRangeInput minVal maxVal input = do
  choice <- readMaybe input :: Maybe Int
  if choice >= minVal && choice <= maxVal then Just choice else Nothing
