module Shared.Logs.FormateNumber (formateNumber, formateFloat) where
    
import Text.Printf (printf)
import Data.List (intersperse)

chunksOf :: Int -> String -> [String]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

formateNumber :: String -> String
formateNumber input =
    let (integerPart, fractionalPart) = break (== '.') input
        formattedInteger = concat . intersperse " " . chunksOf 3 . reverse $ integerPart
        formattedIntegerReversed = reverse formattedInteger
    in if null fractionalPart
        then formattedIntegerReversed
        else formattedIntegerReversed ++ fractionalPart

formateFloat :: Float -> String 
formateFloat num = (formateNumber (printf "%.2f" num))