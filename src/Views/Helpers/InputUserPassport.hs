module Views.Helpers.InputUserPassport (inputUserPassport) where

import Shared.Inputs.InputPassport
import Enteties.Drivers
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
  let editPunkts = ["Продолжить", "Изменить данные паспорта", "Изменить ФИО", "Изменить дату рождения"]
  editPunkt <- chooseData editPunkts (\array -> generateLogData array (\x -> x)) "Выберите пункт для дальнейшего действия: " infoMessage

  case editPunkt of 
    1 -> return (-1)
    _ -> return (editPunkt - 1)

inputUserPassport :: Maybe UserPassport -> Int -> IO (UserPassport)
inputUserPassport oldPassport editStep = do
    (serie, number) <- if editStep == 1 then inputPassport
                    else case oldPassport of 
                        Nothing -> inputPassport
                        Just passport -> return (passportSerie passport, passportNumber passport)

    driver <- getDriverByPassport serie number

    case driver of
        Nothing -> do 
           
            fio <- if editStep == 2 then inputFio
                else case oldPassport of 
                    Nothing -> inputFio
                    Just passport -> return (passportFio passport) 

            (age, date) <- if editStep == 3 then inputDayOfBirth 16 100
                else case oldPassport of 
                    Nothing -> inputDayOfBirth 16 100
                    Just passport -> return (age passport, birthDate passport) 

            let infoMessage = "Паспорт: " ++ (show serie) ++ " " ++ (show number) ++ "\nФИО: " ++ fio ++ "\nДата рождения: " ++ date ++ "\nВозраст: " ++ show age

            updateStep <- choosePassportEditStep infoMessage

            let passport = UserPassport {passportNumber = number, 
                                        passportSerie = serie, 
                                        passportFio = fio, 
                                        age = age, 
                                        birthDate = date, 
                                        drivingLevel = 3,
                                        driver = Nothing
                                        }
            
            case updateStep of 
                -1 -> return passport
                _ -> inputUserPassport (Just passport) updateStep

        Just driver -> do
            userAge <- calcAgeFromDate (Enteties.Drivers.birthday driver)
            
            return UserPassport {passportNumber = (Enteties.Drivers.numberPassport driver), 
                                        passportSerie = (Enteties.Drivers.seriePassport driver), 
                                        passportFio = (Enteties.Drivers.surName driver) ++ " " ++ " " ++ (Enteties.Drivers.firstName driver) ++ " " ++ (Enteties.Drivers.patroName driver), 
                                        age = userAge, 
                                        birthDate = (Enteties.Drivers.birthday driver), 
                                        drivingLevel = (Enteties.Drivers.driverLevel driver),
                                        driver = Just driver
                                        }
                    



    