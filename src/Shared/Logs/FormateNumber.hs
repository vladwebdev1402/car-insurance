module Shared.Logs.FormateNumber (formatNumber) where
    
import Data.List (intersperse)

chunksOf :: Int -> String -> [String]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

formatNumber :: String -> String
formatNumber input =
    let (integerPart, fractionalPart) = break (== '.') input
        formattedInteger = concat . intersperse " " . chunksOf 3 . reverse $ integerPart
        formattedIntegerReversed = reverse formattedInteger
    in if null fractionalPart
        then formattedIntegerReversed
        else formattedIntegerReversed ++ fractionalPart