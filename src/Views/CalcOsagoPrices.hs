module Views.CalcOsagoPrices (calcOsagoPrices) where

import Data.List (sortBy)
import Views.InputOsagoData
import Enteties.TypesKVS
import Enteties.TypesKM
import Enteties.TypesKBM
import Enteties.TypeKS
import Enteties.Drivers
import Enteties.TransportCertificate
import Enteties.TypeKO
import Enteties.CoefTB
import Enteties.Companys
import Enteties.TypesTransport
import Enteties.CompanyPolicyLink
import Enteties.Territories
import Shared.Validators.NothingToJust

calcOsagoPrices :: OsagoUserInfo -> IO [(Company, Float)]
calcOsagoPrices osagoUserInfo = do

  let (age, date) = nothingToJust (Views.InputOsagoData.birthDate osagoUserInfo) "calcOsagoPrices: Ошибка получения возраста и даты"
  
  let drivingExpirience = nothingToJust (Views.InputOsagoData.drivingExpirience osagoUserInfo) "calcOsagoPrices: Ошибка получения опыта вождения"
  
  let territorie = nothingToJust (Views.InputOsagoData.territorie osagoUserInfo) "calcOsagoPrices: Ошибка места проживания"
  
  let autoInfo = nothingToJust (Views.InputOsagoData.autoInfo osagoUserInfo) "calcOsagoPrices: Ошибка информации об автомобиле"
  
  let typeKs = nothingToJust (Views.InputOsagoData.typeKS osagoUserInfo) "calcOsagoPrices: Ошибка получения срока страхования"
  
  let typeKo = nothingToJust (Views.InputOsagoData.typeKO osagoUserInfo) "calcOsagoPrices: Ошибка получения количества водителей"
  
  let (enginePower, transportBrand, transportModel, transport, category, certificate) = autoInfo

  coefKVS <- getTypeKVS age drivingExpirience True

  let coefKT = (Enteties.Territories.coefOsago territorie)

  coefKM <- getTypeKMByPower enginePower True

  driver <- maybe (return Nothing) (\cert -> fmap Just (getDriverById (Enteties.TransportCertificate.driverId cert))) certificate

  driverLevel <- maybe (return 3) (\user -> return (Enteties.Drivers.driverLevel user)) driver
  
  coefKBM <- getTypeKBMByDriverLever driverLevel

  let summuryCoef = (Enteties.TypeKS.coefOsago typeKs) * (Enteties.TypeKO.coefOsago typeKo) * (Enteties.TypesKVS.coefOsago coefKVS) * coefKT * (Enteties.TypesKM.coefOsago coefKM) * (Enteties.TypesKBM.coefOsago coefKBM )

  let policyId = 0

  links <- getCompanyPolicyLinkByPolicy policyId

  coefsTb <- mapM (\link -> getCoefTB (companyId link) (Enteties.TypesTransport.uid category)) links

  companys <- mapM (\link -> getCompanyById (companyId link)) links

  companysWithPrice <- mapM (\(company, coef) -> return (company, (maybe (0) Enteties.CoefTB.value coef) * summuryCoef)) (zip companys coefsTb)

  let filteredCompanys = filter (\(_, price) -> price > 0) companysWithPrice  

  return $ sortBy (\(_, priceX) (_, priceY) -> compare priceX priceY) filteredCompanys
