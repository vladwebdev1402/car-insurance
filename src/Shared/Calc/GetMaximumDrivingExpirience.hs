module Shared.Calc.GetMaximumDrivingExpirience (getMaximumDrivingExpirience) where

getMaximumDrivingExpirience :: Int -> Int
getMaximumDrivingExpirience age
  | age <= 21 = 6
  | age <= 24 = 9
  | age <= 29 = 14
  | otherwise = 100