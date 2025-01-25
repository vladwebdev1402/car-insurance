module Views.Helpers.ConfirmIdentity (confirmIdentity) where

import System.Process (callCommand)
import Enteties.Drivers
import Shared.Inputs.InputPassport
import Shared.Logs.Console

confirmIdentity :: Driver -> IO Bool
confirmIdentity driver = do
    (serie, number) <- inputPassport "Подтвердите вашу личность. Чтобы вернуться в меню введите \"выход\":"
    if (serie == -1 && number == -1) then return False
    else if (serie == (Enteties.Drivers.seriePassport driver) && number == (Enteties.Drivers.numberPassport driver)) then return True
    else do 
        callCommand "cls"
        consoleError "Введённые данные не соответствуют водителю в базе данных."
        confirmIdentity driver
    
