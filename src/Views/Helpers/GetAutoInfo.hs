module Views.Helpers.GetAutoInfo (getAutoInfo) where

import Enteties.TransportBrands 
import Enteties.TransportModels 
import Enteties.Transports
import Enteties.TypesTransport

getAutoInfo :: Int-> Maybe TransportBrand-> Maybe TransportModel-> Maybe Transport-> TypeTransport -> String
getAutoInfo enginePower transportBrand transportModel transport typeTransport = 
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
    "\nКатегория автомобиля: " ++ (Enteties.TypesTransport.description typeTransport)
