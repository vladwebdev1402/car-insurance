module Shared.Helpers.GetTodayDate (getTodayDate) where

import Data.Time

getTodayDate :: IO String
getTodayDate = do
    currentTime <- getCurrentTime  
    let today = utctDay currentTime  
    return $ formatTime defaultTimeLocale "%d.%m.%Y" today 