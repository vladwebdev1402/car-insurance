module Views.CalcDsagoPrices (calcDsagoPrices) where

import System.Process (callCommand)
import Data.List (sortBy)
import Views.UserInfo
import Enteties.Companys
import Shared.Validators.NothingToJust
import Enteties.TypesKVS
import Enteties.TypesKM
import Enteties.TypesKBM
import Enteties.TypeKS
import Enteties.TypeKO
import Enteties.CoefTB
import Enteties.CoefKBM
import Enteties.CoefKM
import Enteties.CoefKO
import Enteties.CoefKS
import Enteties.CoefKT
import Enteties.CoefKVS
import Enteties.CoefAdditional
import Enteties.Companys
import Enteties.TypesTransport
import Enteties.CompanyPolicyLink
import Enteties.Territories
import Enteties.TransportCertificate 
import Enteties.Drivers
import Enteties.CoefAdditional
import Enteties.Additional

calcDsagoPrices :: UserInfo -> IO [(Company, Float)]
calcDsagoPrices dsagoUserInfo = do 
    let (age, date) = nothingToJust (Views.UserInfo.birthDate dsagoUserInfo) "calcDsagoPrice: Ошибка получения возраста и даты"
    
    let drivingExpirience = nothingToJust (Views.UserInfo.drivingExpirience dsagoUserInfo) "calcDsagoPrice: Ошибка получения опыта вождения"
    
    let territorie = nothingToJust (Views.UserInfo.territorie dsagoUserInfo) "calcDsagoPrice: Ошибка места проживания"
  
    let autoInfo = nothingToJust (Views.UserInfo.autoInfo dsagoUserInfo) "calcDsagoPrice: Ошибка информации об автомобиле"

    let (enginePower, transportBrand, transportModel, transport, category, certificate) = autoInfo
  
    let typeKs = nothingToJust (Views.UserInfo.typeKS dsagoUserInfo) "calcDsagoPrice: Ошибка получения срока страхования"
  
    let typeKo = nothingToJust (Views.UserInfo.typeKO dsagoUserInfo) "calcDsagoPrice: Ошибка получения количества водителей"

    let additional = nothingToJust (Views.UserInfo.additional dsagoUserInfo) "calcDsagoPrice: Ошибка получения дополнительной суммы"

    let policyId = 2

    driver <- maybe (return Nothing) (\cert -> fmap Just (getDriverById (Enteties.TransportCertificate.driverId cert))) certificate

    driverLevel <- maybe (return 3) (\user -> return (Enteties.Drivers.driverLevel user)) driver

    links <- getCompanyPolicyLinkByPolicy policyId

    companysWithPrice <- mapM (\link -> do
        let linkId = (Enteties.CompanyPolicyLink.uid link)

        typeKvs <- getTypeKVS age drivingExpirience False

        typeKbm <- getTypeKBMByDriverLever driverLevel

        typeKm <- getTypeKMByPower enginePower False

        company <- getCompanyById (Enteties.CompanyPolicyLink.companyId link)

        coefKvs <- getCoefKVS linkId (Enteties.TypesKVS.uid typeKvs)

        coefAdditional <- getCoefAdditional linkId (Enteties.Additional.uid additional)

        coefKbm <- getCoefKBM linkId (Enteties.TypesKBM.uid typeKbm)

        coefKm <- getCoefKM linkId (Enteties.TypesKM.uid typeKm)

        coefKo <- getCoefKO linkId (Enteties.TypeKO.uid typeKo)

        coefKs <- getCoefKS linkId (Enteties.TypeKS.uid typeKs)

        coefKt <- getCoefKT linkId (Enteties.Territories.uid territorie)

        coefKvs <- getCoefKVS linkId (Enteties.TypesKVS.uid typeKvs)

        coefTb <- getCoefTB linkId (Enteties.TypesTransport.uid category)

        let price = (maybe (0) Enteties.CoefKVS.value coefKvs)
                    * (maybe (0) Enteties.CoefKBM.value coefKbm) 
                    * (maybe (0) Enteties.CoefKM.value coefKm) 
                    * (maybe (0) Enteties.CoefKO.value coefKo) 
                    * (maybe (0) Enteties.CoefKS.value coefKs) 
                    * (maybe (0) Enteties.CoefKT.value coefKt) 
                    * (maybe (0) Enteties.CoefAdditional.value coefAdditional) 
                    * (maybe (0) Enteties.CoefTB.value coefTb)

        return (company, price)
        ) links

    let filteredCompanys = filter (\(_, price) -> price > 0) companysWithPrice  

    return $ sortBy (\(_, priceX) (_, priceY) -> compare priceX priceY) filteredCompanys

        