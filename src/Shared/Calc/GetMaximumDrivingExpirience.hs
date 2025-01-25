module Shared.Calc.GetMaximumDrivingExpirience (getMaximumDrivingExpirience) where

getMaximumDrivingExpirience :: Int -> Int
getMaximumDrivingExpirience age
  | age <= 29 = age - 16
  | otherwise = 100