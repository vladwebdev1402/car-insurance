module Shared.Calc.CalcDaysSince (calcDaysSince) where

import Data.Time
import Data.Time.Format
import Data.Time.LocalTime

parseDate :: String -> Day
parseDate dateStr = parseTimeOrError True defaultTimeLocale "%d.%m.%Y" dateStr

calcDaysSince :: String -> IO Int
calcDaysSince dateStr = do
    let pastDate = parseDate dateStr  
    currentDate <- utctDay <$> getCurrentTime 
    return $ fromIntegral (diffDays currentDate pastDate)