module Views.CalcKaskoPrices (calcKaskoPrices) where

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
import Enteties.CoefDeductible
import Enteties.CoefService
import Enteties.Companys
import Enteties.TypesTransport
import Enteties.CompanyPolicyLink
import Enteties.Territories
import Enteties.TransportCertificate 
import Enteties.Drivers
import Enteties.Deductibles

calcKaskoPrices :: UserInfo -> IO [(Company, Float)]
calcKaskoPrices kaskoUserInfo = do 
    let (age, date) = nothingToJust (Views.UserInfo.birthDate kaskoUserInfo) "calcKaskoPrices: Ошибка получения возраста и даты"
    
    let drivingExpirience = nothingToJust (Views.UserInfo.drivingExpirience kaskoUserInfo) "calcKaskoPrices: Ошибка получения опыта вождения"
    
    let territorie = nothingToJust (Views.UserInfo.territorie kaskoUserInfo) "calcKaskoPrices: Ошибка места проживания"
  
    let autoInfo = nothingToJust (Views.UserInfo.autoInfo kaskoUserInfo) "calcKaskoPrices: Ошибка информации об автомобиле"

    let (enginePower, transportBrand, transportModel, transport, category, certificate) = autoInfo
  
    let typeKs = nothingToJust (Views.UserInfo.typeKS kaskoUserInfo) "calcKaskoPrices: Ошибка получения срока страхования"
  
    let typeKo = nothingToJust (Views.UserInfo.typeKO kaskoUserInfo) "calcKaskoPrices: Ошибка получения количества водителей"

    let services = nothingToJust (Views.UserInfo.policyServices kaskoUserInfo) "calcKaskoPrices: Ошибка получения сервисов"
    
    let deductible = nothingToJust (Views.UserInfo.deductible kaskoUserInfo) "calcKaskoPrices: Ошибка получения франшизы"

    let policyId = 1

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

        coefDeductible <- getCoefDeductible linkId (Enteties.Deductibles.uid deductible)

        summuryServiceCoef <- getSummuryCoefService linkId services

        coefKbm <- getCoefKBM linkId (Enteties.TypesKBM.uid typeKbm)

        coefKm <- getCoefKM linkId (Enteties.TypesKM.uid typeKm)

        coefKo <- getCoefKO linkId (Enteties.TypeKO.uid typeKo)

        coefKs <- getCoefKS linkId (Enteties.TypeKS.uid typeKs)

        coefKt <- getCoefKT linkId (Enteties.Territories.uid territorie)

        coefKvs <- getCoefKVS linkId (Enteties.TypesKVS.uid typeKvs)

        coefTb <- getCoefTB linkId (Enteties.TypesTransport.uid category)

        let price = (maybe (0) Enteties.CoefKVS.value coefKvs)
                    * (maybe (0) Enteties.CoefDeductible.value coefDeductible) 
                    * (maybe (0) Enteties.CoefKBM.value coefKbm) 
                    * (maybe (0) Enteties.CoefKM.value coefKm) 
                    * (maybe (0) Enteties.CoefKO.value coefKo) 
                    * (maybe (0) Enteties.CoefKS.value coefKs) 
                    * (maybe (0) Enteties.CoefKT.value coefKt) 
                    * (maybe (0) Enteties.CoefTB.value coefTb) 
                    * summuryServiceCoef

        return (company, price)
        ) links

    let filteredCompanys = filter (\(_, price) -> price > 0) companysWithPrice  

    return $ sortBy (\(_, priceX) (_, priceY) -> compare priceX priceY) filteredCompanys

        