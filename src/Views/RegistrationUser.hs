module Views.RegistrationUser (registrationUser) where

import System.Process (callCommand)
import Data.List.Split (splitOn) 
import Shared.Inputs.ChooseData (chooseData)
import Shared.Logs.LogData
import Shared.Inputs.InputPassport
import Shared.Inputs.InputFio
import Shared.Inputs.InputDayOfBirth
import Shared.Calc.GetMaximumDrivingExpirience
import Shared.Inputs.InputRangeNumber
import Entities.Drivers


registrationUser :: IO ()
registrationUser = do 
    let punkts = ["Зарегестрировать данные паспорта", "Зарегестрировать транспорт"]
    index <- chooseData punkts (\array -> generateLogData array (\item -> item)) "\nВыберите выберите пункт меню:" ""


    case index of
        1 -> registrationDriver ""
        2 -> registrationTransport
        _ -> return ()

registrationTransport :: IO ()
registrationTransport = do
    putStrLn ""

registrationDriver :: String -> IO ()
registrationDriver infoMessage = do
    putStrLn infoMessage

    (serie, number) <- inputPassport ""

    checkDriver serie number

    
    case serie of 
        -1 -> return ()
        _ -> do
            (age, dayOfBirth) <- inputDayOfBirth 16 100
            fio <- inputFio
            let [surName, firstName, patroName] = splitOn " " fio
            drivingExpirience <- inputRangeNumber "" "Введите стаж вождения: " 0 (getMaximumDrivingExpirience age)
            addNewDriver (Driver {
                Entities.Drivers.uid = 0, 
                Entities.Drivers.surName = surName, 
                Entities.Drivers.firstName = firstName, 
                Entities.Drivers.patroName = patroName, 
                Entities.Drivers.experience = drivingExpirience, 
                Entities.Drivers.driverLevel = 3, 
                Entities.Drivers.numberPassport = number, 
                Entities.Drivers.seriePassport = serie, 
                Entities.Drivers.birthday = dayOfBirth
            })

            return ()

checkDriver :: Int -> Int -> IO ()
checkDriver serie number = do
    driver <- getDriverByPassport serie number
    case driver of 
        Nothing -> return ()
        _ -> do
            callCommand "cls"
            registrationDriver "Данный паспорт уже существует в системе. Нажмите Enter, чтобы продолжить"
            return ()
        
            
