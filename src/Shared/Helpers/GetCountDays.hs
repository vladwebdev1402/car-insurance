module Shared.Helpers.GetCountDays (getCountDaysFromMonths) where

getCountDaysFromMonths  :: Float -> Int
getCountDaysFromMonths countMonths = round $ countMonths * 30    
  