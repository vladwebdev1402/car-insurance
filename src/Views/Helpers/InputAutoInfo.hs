module Views.Helpers.InputAutoInfo (inputAutoInfo) where

import System.Process (callCommand)
import Entities.TransportBrands 
import Entities.TransportModels 
import Entities.Transports
import Entities.TypesTransport
import Entities.TransportCertificate
import Modules.ChooseTransportBrand
import Modules.ChooseTransportModel
import Modules.ChooseTransport
import Modules.ChooseTypeTransport
import Shared.Inputs.InputRangeNumber
import Shared.Inputs.InputNumberRegistration
import Shared.Logs.Console

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
            transportModel <- chooseTransportModel (Entities.TransportBrands.uid transportBrand) infoMessage
            transport <- chooseTransport (Entities.TransportModels.uid transportModel) infoMessage
            return ((Entities.Transports.power transport), Just transportBrand, Just transportModel, Just transport)
    
    category <- case transport of
        Nothing -> do
            callCommand "cls"
            category <- chooseTypeTransport infoMessage
            return category
        Just transport -> getTypeTransportById (Entities.Transports.typeTransportId transport)
    
    return (enginePower, transportBrand, transportModel, transport, category, Nothing)


inputAutoInfo True infoMessage = do
    registrationNumber <- inputNumberRegistration infoMessage
    certificate <- getTransportCertificateByNumber registrationNumber

    case registrationNumber of 
        "выход" -> return (0, Nothing, Nothing, Nothing, getNullTypeTransport, Nothing)
        _ -> do
            case certificate of
                Nothing -> do
                    callCommand "cls"
                    consoleError "Ошибка: автомобиль с таким регистрационным номером не найден в базе данных."
                    inputAutoInfo True infoMessage
                Just certificate -> do 
                    transport <- getTransportById (Entities.TransportCertificate.transportId certificate)
                    category <- getTypeTransportById (Entities.Transports.typeTransportId transport) 
                    model <- getTransportModelById (Entities.Transports.transportModelId transport)
                    brand <- getTransportBrandById (Entities.TransportModels.transportBrandId model)
                    return ((Entities.Transports.power transport), Just brand, Just model, Just transport, category, Just certificate)
        