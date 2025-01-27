module Modules.InputUserPassport (UserPassport(..), inputUserPassport) where

import Shared.Inputs.InputPassport
import Entities.Drivers
import Shared.Logs.Console
import Shared.Inputs.InputDayOfBirth
import Shared.Inputs.InputFio
import Shared.Inputs.ChooseData
import Shared.Calc.CalcAgeFromDate
import Shared.Logs.LogData

data UserPassport = UserPassport {
    passportNumber :: Int,
    passportSerie :: Int,
    age :: Int,
    passportFio :: String,
    birthDate :: String,
    drivingLevel :: Int,
    driver :: Maybe Driver
} deriving (Read, Show)

choosePassportEditStep :: String -> IO (Int)
choosePassportEditStep infoMessage = do
  let editPunkts = ["Продолжить", "Изменить данные паспорта"]
  editPunkt <- chooseData editPunkts (\array -> generateLogData array (\x -> x)) "Проверьте данные и выберите пункт для дальнейшего действия: " infoMessage

  case editPunkt of 
    1 -> return (-1)
    _ -> return (editPunkt - 1)

inputUserPassport :: Maybe UserPassport -> Int -> IO (UserPassport)
inputUserPassport oldPassport editStep = do
    (serie, number) <- if editStep == 1 then inputPassport ""
                    else case oldPassport of 
                        Nothing -> inputPassport ""
                        Just passport -> return (passportSerie passport, passportNumber passport)

    driver <- getDriverByPassport serie number

    case driver of
        Nothing -> do 
            consoleError "Водитель не найден в базе данных."
            inputUserPassport oldPassport editStep
            -- fio <- if editStep == 2 then inputFio
            --     else case oldPassport of 
            --         Nothing -> inputFio
            --         Just passport -> return (passportFio passport) 

            -- (age, date) <- if editStep == 3 then inputDayOfBirth 16 100
            --     else case oldPassport of 
            --         Nothing -> inputDayOfBirth 16 100
            --         Just passport -> return (age passport, birthDate passport) 

            -- let infoMessage = "Паспорт: " ++ (show serie) ++ " " ++ (show number) ++ "\nФИО: " ++ fio ++ "\nДата рождения: " ++ date ++ "\nВозраст: " ++ show age

            -- updateStep <- choosePassportEditStep infoMessage

            -- let passport = UserPassport {passportNumber = number, 
            --                             passportSerie = serie, 
            --                             passportFio = fio, 
            --                             age = age, 
            --                             birthDate = date, 
            --                             drivingLevel = 3,
            --                             driver = Nothing
            --                             }
            
            -- case updateStep of 
            --     -1 -> return passport
            --     _ -> inputUserPassport (Just passport) updateStep

        Just driver -> do
            userAge <- calcAgeFromDate (Entities.Drivers.birthday driver)
            
            return UserPassport {passportNumber = (Entities.Drivers.numberPassport driver), 
                                        passportSerie = (Entities.Drivers.seriePassport driver), 
                                        passportFio = (Entities.Drivers.surName driver) ++ " " ++ " " ++ (Entities.Drivers.firstName driver) ++ " " ++ (Entities.Drivers.patroName driver), 
                                        age = userAge, 
                                        birthDate = (Entities.Drivers.birthday driver), 
                                        drivingLevel = (Entities.Drivers.driverLevel driver),
                                        driver = Just driver
                                        }
                    



    