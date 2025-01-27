module Views.CalcDsagoPrices (calcDsagoPrices) where

import System.Process (callCommand)
import Data.List (sortBy)
import Views.UserInfo
import Entities.Companys
import Shared.Validators.NothingToJust
import Entities.TypesKVS
import Entities.TypesKM
import Entities.TypesKBM
import Entities.TypeKS
import Entities.TypeKO
import Entities.CoefTB
import Entities.CoefKBM
import Entities.CoefKM
import Entities.CoefKO
import Entities.CoefKS
import Entities.CoefKT
import Entities.CoefKVS
import Entities.CoefAdditional
import Entities.Companys
import Entities.TypesTransport
import Entities.CompanyPolicyLink
import Entities.Territories
import Entities.TransportCertificate 
import Entities.Drivers
import Entities.CoefAdditional
import Entities.Additional

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

    driver <- maybe (return Nothing) (\cert -> fmap Just (getDriverById (Entities.TransportCertificate.driverId cert))) certificate

    driverLevel <- maybe (return 3) (\user -> return (Entities.Drivers.driverLevel user)) driver

    links <- getCompanyPolicyLinkByPolicy policyId

    companysWithPrice <- mapM (\link -> do
        let linkId = (Entities.CompanyPolicyLink.uid link)

        typeKvs <- getTypeKVS age drivingExpirience False

        typeKbm <- getTypeKBMByDriverLever driverLevel

        typeKm <- getTypeKMByPower enginePower False

        company <- getCompanyById (Entities.CompanyPolicyLink.companyId link)

        coefKvs <- getCoefKVS linkId (Entities.TypesKVS.uid typeKvs)

        coefAdditional <- getCoefAdditional linkId (Entities.Additional.uid additional)

        coefKbm <- getCoefKBM linkId (Entities.TypesKBM.uid typeKbm)

        coefKm <- getCoefKM linkId (Entities.TypesKM.uid typeKm)

        coefKo <- getCoefKO linkId (Entities.TypeKO.uid typeKo)

        coefKs <- getCoefKS linkId (Entities.TypeKS.uid typeKs)

        coefKt <- getCoefKT linkId (Entities.Territories.uid territorie)

        coefKvs <- getCoefKVS linkId (Entities.TypesKVS.uid typeKvs)

        coefTb <- getCoefTB linkId (Entities.TypesTransport.uid category)

        let price = (maybe (0) Entities.CoefKVS.value coefKvs)
                    * (maybe (0) Entities.CoefKBM.value coefKbm) 
                    * (maybe (0) Entities.CoefKM.value coefKm) 
                    * (maybe (0) Entities.CoefKO.value coefKo) 
                    * (maybe (0) Entities.CoefKS.value coefKs) 
                    * (maybe (0) Entities.CoefKT.value coefKt) 
                    * (maybe (0) Entities.CoefAdditional.value coefAdditional) 
                    * (maybe (0) Entities.CoefTB.value coefTb)

        return (company, price)
        ) links

    let filteredCompanys = filter (\(_, price) -> price > 0) companysWithPrice  

    return $ sortBy (\(_, priceX) (_, priceY) -> compare priceX priceY) filteredCompanys

        