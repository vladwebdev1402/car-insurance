module Views.Helpers.ConfirmIdentity (confirmIdentity) where

import System.Process (callCommand)
import Entities.Drivers
import Shared.Inputs.InputPassport
import Shared.Logs.Console

confirmIdentity :: Driver -> IO Bool
confirmIdentity driver = do
    (serie, number) <- inputPassport "Подтвердите вашу личность. Чтобы вернуться в меню введите \"выход\":"
    if (serie == -1 && number == -1) then return False
    else if (serie == (Entities.Drivers.seriePassport driver) && number == (Entities.Drivers.numberPassport driver)) then return True
    else do 
        callCommand "clear"
        consoleError "Введённые данные не соответствуют водителю в базе данных."
        confirmIdentity driver
    
