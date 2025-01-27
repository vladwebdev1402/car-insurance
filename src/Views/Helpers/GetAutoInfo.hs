module Views.Helpers.GetAutoInfo (getAutoInfo) where

import Entities.TransportBrands 
import Entities.TransportModels 
import Entities.Transports
import Entities.TypesTransport
import Entities.TransportCertificate

getAutoInfo :: Int -> Maybe TransportBrand -> Maybe TransportModel -> Maybe Transport -> TypeTransport -> Maybe TransportCertificate -> String
getAutoInfo enginePower transportBrand transportModel transport typeTransport certificate = 
    "\nМощность двигателя: " ++ show enginePower ++ " л.с." ++ 
    (case transportBrand of
        Nothing -> ""
        Just brand -> "\nМарка автомобиля: " ++ (Entities.TransportBrands.name brand)) ++
    (case transportModel of
        Nothing -> ""
        Just model -> "\nМодель автомобиля: " ++ (Entities.TransportModels.name model)) ++ 
    (case transport of
        Nothing -> ""
        Just tr -> "\nГод выпуска автомобиля: " ++ (show (Entities.Transports.year tr))) ++
    (case certificate of
        Nothing -> ""
        Just cert -> "\nРегистрационный номер: " ++ (Entities.TransportCertificate.registrationNumber cert)) ++
    "\nКатегория автомобиля: " ++ (Entities.TypesTransport.description typeTransport)
