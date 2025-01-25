module Views.Helpers.GetAutoInfo (getAutoInfo) where

import Enteties.TransportBrands 
import Enteties.TransportModels 
import Enteties.Transports
import Enteties.TypesTransport
import Enteties.TransportCertificate

getAutoInfo :: Int -> Maybe TransportBrand -> Maybe TransportModel -> Maybe Transport -> TypeTransport -> Maybe TransportCertificate -> String
getAutoInfo enginePower transportBrand transportModel transport typeTransport certificate = 
    "\nМощность двигателя: " ++ show enginePower ++ " л.с." ++ 
    (case transportBrand of
        Nothing -> ""
        Just brand -> "\nМарка автомобиля: " ++ (Enteties.TransportBrands.name brand)) ++
    (case transportModel of
        Nothing -> ""
        Just model -> "\nМодель автомобиля: " ++ (Enteties.TransportModels.name model)) ++ 
    (case transport of
        Nothing -> ""
        Just tr -> "\nГод выпуска автомобиля: " ++ (show (Enteties.Transports.year tr))) ++
    (case certificate of
        Nothing -> ""
        Just cert -> "\nРегистрационный номер: " ++ (Enteties.TransportCertificate.registrationNumber cert)) ++
    "\nКатегория автомобиля: " ++ (Enteties.TypesTransport.description typeTransport)
