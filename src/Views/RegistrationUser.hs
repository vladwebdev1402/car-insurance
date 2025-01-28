module Views.RegistrationUser (registrationUser) where

import System.Process (callCommand)
import Data.List.Split (splitOn) 
import Shared.Inputs.ChooseData (chooseData)
import Shared.Logs.LogData
import Shared.Inputs.InputPassport
import Shared.Inputs.InputFio
import Shared.Inputs.InputDayOfBirth
import Shared.Calc.GetMaximumDrivingExpirience
import Shared.Inputs.InputNumberRegistration
import Shared.Inputs.InputRangeNumber
import Entities.Drivers
import Entities.TransportCertificate
import Modules.ChooseTransportBrand
import Modules.ChooseTransportModel
import Modules.ChooseTransport
import Entities.TransportBrands 
import Entities.TransportModels 
import Entities.Transports


registrationUser :: IO ()
registrationUser = do 
    let punkts = ["Зарегистрировать данные паспорта", "Зарегистрировать транспорт"]
    index <- chooseData punkts (\array -> generateLogData array (\item -> item)) "\nВыберите выберите пункт меню:" ""


    case index of
        1 -> registrationDriver ""
        2 -> registrationTransport ""
        _ -> return ()

registrationTransport :: String -> IO ()
registrationTransport infoMessage = do

    putStrLn infoMessage

    regNum <- inputNumberRegistration ""


    case regNum of 
        "выход" -> return ()
        _ -> do 
            checkRegNum regNum
            transportBrand <- chooseTransportBrand infoMessage
            transportModel <- chooseTransportModel (Entities.TransportBrands.uid transportBrand) infoMessage
            transport <- chooseTransport (Entities.TransportModels.uid transportModel) infoMessage
            driver <- getDriver ""
            let cert = TransportCertificate {Entities.TransportCertificate.uid = 0, 
                    Entities.TransportCertificate.transportId = (Entities.Transports.uid transport),
                    Entities.TransportCertificate.driverId = (Entities.Drivers.uid driver),
                    Entities.TransportCertificate.registrationNumber = regNum
                    }
            addNewTransportCertificates cert
            return ()

getDriver :: String -> IO Driver 
getDriver info = do
    putStrLn info
    (serie, number) <- inputPassport ""
    driver <- getDriverByPassport serie number
    case driver of 
        Nothing -> do
            getDriver "Водитель не найден в бд"
        Just driv -> return driv

checkRegNum :: String -> IO ()
checkRegNum regNum = do
    cert <- getTransportCertificateByNumber regNum

    case cert of 
        Nothing -> return ()
        _ -> do 
            callCommand "cls"
            registrationTransport "Автомобиль с таким номером уже зарегистрирован"

registrationDriver :: String -> IO ()
registrationDriver infoMessage = do
    putStrLn infoMessage

    (serie, number) <- inputPassport ""

    checkDriver serie number
    
    case serie of 
        -1 -> return ()
        _ -> do
            (age, dayOfBirth) <- inputDayOfBirth 16 100

            drivingExpirience <- inputRangeNumber "" "Введите стаж вождения: " 0 (getMaximumDrivingExpirience age)

            fio <- inputFio

            let [surName, firstName, patroName] = splitOn " " fio

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
            registrationDriver "Данный паспорт уже существует в системе"
            return ()
        
            
