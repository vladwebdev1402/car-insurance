module Views.Helpers.InputAutoInfo (inputAutoInfo) where

import System.Process (callCommand)
import Enteties.TransportBrands 
import Enteties.TransportModels 
import Enteties.Transports
import Enteties.TypesTransport
import Enteties.TransportCertificate
import Modules.ChooseTransportBrand
import Modules.ChooseTransportModel
import Modules.ChooseTransport
import Modules.ChooseTypeTransport
import Shared.Inputs.InputRangeNumber
import Shared.Inputs.InputNumberRegistration
import Shared.Logs.Console

-- булеан оформление
inputAutoInfo :: Bool -> String -> IO ((Int, Maybe TransportBrand, Maybe TransportModel, Maybe Transport, TypeTransport, Maybe TransportCertificate))
inputAutoInfo False infoMessage = do
    autoInfoMode <- inputRangeNumber infoMessage "Выберите режим ввода данных для информации об автомобиле:\n1. Ввод с клавиатуры\n2. Поиск автомобиля из базы данных" 1 2
    
    (enginePower, transportBrand, transportModel, transport) <- case autoInfoMode of
        1 -> do
            callCommand "cls" 
            enginePower <- inputRangeNumber infoMessage "Введите мощность двигателя: " 16 450
            return (enginePower, Nothing, Nothing, Nothing)
        2 -> do 
            callCommand "cls"
            transportBrand <- chooseTransportBrand infoMessage
            transportModel <- chooseTransportModel (Enteties.TransportBrands.uid transportBrand) infoMessage
            transport <- chooseTransport (Enteties.TransportModels.uid transportModel) infoMessage
            return ((Enteties.Transports.power transport), Just transportBrand, Just transportModel, Just transport)
    
    category <- case transport of
        Nothing -> do
            callCommand "cls"
            category <- chooseTypeTransport infoMessage
            return category
        Just transport -> getTypeTransportById (Enteties.Transports.typeTransportId transport)
    
    return (enginePower, transportBrand, transportModel, transport, category, Nothing)


inputAutoInfo True infoMessage = do
    registrationNumber <- inputNumberRegistration infoMessage
    certificate <- getTransportCertificateByNumber registrationNumber

    case certificate of
        Nothing -> do
            callCommand "cls"
            consoleError "Ошибка: автомобиль с таким регистрационным номером не найден в базе данных."
            inputAutoInfo True infoMessage
        Just certificate -> do 
            transport <- getTransportById (Enteties.TransportCertificate.transportId certificate)
            category <- getTypeTransportById (Enteties.Transports.typeTransportId transport) 
            model <- getTransportModelById (Enteties.Transports.transportModelId transport)
            brand <- getTransportBrandById (Enteties.TransportModels.transportBrandId model)
            return ((Enteties.Transports.power transport), Just brand, Just model, Just transport, category, Just certificate)
        