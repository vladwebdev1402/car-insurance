module Views.Helpers.InputAutoInfo (inputAutoInfo) where

import System.Process (callCommand)
import Enteties.TransportBrands 
import Enteties.TransportModels 
import Enteties.Transports
import Enteties.TypesTransport
import Modules.ChooseTransportBrand
import Modules.ChooseTransportModel
import Modules.ChooseTransport
import Modules.ChooseTypeTransport
import Shared.Inputs.InputRangeNumber

-- булеан оформление
inputAutoInfo :: Bool -> IO ((Int, Maybe TransportBrand, Maybe TransportModel, Maybe Transport, TypeTransport))
inputAutoInfo False = do
    autoInfoMode <- inputRangeNumber "Выберите режим ввода данных для информации об автомобиле:\n1. Ввод с клавиатуры\n2. Поиск автомобиля из базы данных" "" 1 2
    
    (enginePower, transportBrand, transportModel, transport) <- case autoInfoMode of
        1 -> do
            callCommand "cls" 
            enginePower <- inputRangeNumber "Введите мощность двигателя: " "" 16 450
            return (enginePower, Nothing, Nothing, Nothing)
        2 -> do 
            callCommand "cls"
            transportBrand <- chooseTransportBrand
            transportModel <- chooseTransportModel (Enteties.TransportBrands.uid transportBrand)
            transport <- chooseTransport (Enteties.TransportModels.uid transportModel)
            return ((Enteties.Transports.power transport), Just transportBrand, Just transportModel, Just transport)
    
    category <- case transport of
        Nothing -> do
            callCommand "cls"
            category <- chooseTypeTransport
            return category
        Just transport -> getTypeTransportById (Enteties.Transports.typeTransportId transport)
    
    return (enginePower, transportBrand, transportModel, transport, category)


inputAutoInfo True = do
    callCommand "cls"
    transportBrand <- chooseTransportBrand
    transportModel <- chooseTransportModel (Enteties.TransportBrands.uid transportBrand)
    transport <- chooseTransport (Enteties.TransportModels.uid transportModel)
    category <- getTypeTransportById (Enteties.Transports.typeTransportId transport)
    return (Enteties.Transports.power transport, Just transportBrand, Just transportModel, Just transport, category)