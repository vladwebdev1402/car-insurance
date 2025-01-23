module Shared.Calc.GetMaximumDrivingExpirience (getMaximumDrivingExpirience) where

getMaximumDrivingExpirience :: Int -> Int
getMaximumDrivingExpirience age
  | age <= 29 = age - 15
  | otherwise = 100