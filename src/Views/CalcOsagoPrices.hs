module Views.CalcOsagoPrices (calcOsagoPrices) where

import Data.List (sortBy)
import Views.UserInfo
import Entities.TypesKVS
import Entities.TypesKM
import Entities.TypesKBM
import Entities.TypeKS
import Entities.Drivers
import Entities.TransportCertificate
import Entities.TypeKO
import Entities.CoefTB
import Entities.Companys
import Entities.TypesTransport
import Entities.CompanyPolicyLink
import Entities.Territories
import Shared.Validators.NothingToJust
import Views.UserInfo

calcOsagoPrices :: UserInfo -> IO [(Company, Float)]
calcOsagoPrices osagoUserInfo = do

  let (age, date) = nothingToJust (Views.UserInfo.birthDate osagoUserInfo) "calcOsagoPrices: Ошибка получения возраста и даты"
  
  let drivingExpirience = nothingToJust (Views.UserInfo.drivingExpirience osagoUserInfo) "calcOsagoPrices: Ошибка получения опыта вождения"
  
  let territorie = nothingToJust (Views.UserInfo.territorie osagoUserInfo) "calcOsagoPrices: Ошибка места проживания"
  
  let autoInfo = nothingToJust (Views.UserInfo.autoInfo osagoUserInfo) "calcOsagoPrices: Ошибка информации об автомобиле"
  
  let typeKs = nothingToJust (Views.UserInfo.typeKS osagoUserInfo) "calcOsagoPrices: Ошибка получения срока страхования"
  
  let typeKo = nothingToJust (Views.UserInfo.typeKO osagoUserInfo) "calcOsagoPrices: Ошибка получения количества водителей"
  
  let (enginePower, transportBrand, transportModel, transport, category, certificate) = autoInfo

  coefKVS <- getTypeKVS age drivingExpirience True

  let coefKT = (Entities.Territories.coefOsago territorie)

  coefKM <- getTypeKMByPower enginePower True

  driver <- maybe (return Nothing) (\cert -> fmap Just (getDriverById (Entities.TransportCertificate.driverId cert))) certificate

  driverLevel <- maybe (return 3) (\user -> return (Entities.Drivers.driverLevel user)) driver
  
  coefKBM <- getTypeKBMByDriverLever driverLevel

  let summuryCoef = (Entities.TypeKS.coefOsago typeKs) * (Entities.TypeKO.coefOsago typeKo) * (Entities.TypesKVS.coefOsago coefKVS) * coefKT * (Entities.TypesKM.coefOsago coefKM) * (Entities.TypesKBM.coefOsago coefKBM )

  let policyId = 0

  links <- getCompanyPolicyLinkByPolicy policyId

  coefsTb <- mapM (\link -> getCoefTB (companyId link) (Entities.TypesTransport.uid category)) links

  companys <- mapM (\link -> getCompanyById (companyId link)) links

  companysWithPrice <- mapM (\(company, coef) -> return (company, (maybe (0) Entities.CoefTB.value coef) * summuryCoef)) (zip companys coefsTb)

  let filteredCompanys = filter (\(_, price) -> price > 0) companysWithPrice  

  return $ sortBy (\(_, priceX) (_, priceY) -> compare priceX priceY) filteredCompanys
