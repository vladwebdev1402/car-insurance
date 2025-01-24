module Views.CalcOsagoPrices (calcOsagoPrices) where

import Data.List (sortBy)
import Views.InputOsagoData
import Enteties.TypesKVS
import Enteties.TypesKM
import Enteties.TypesKBM
import Enteties.TypeKS
import Enteties.TypeKO
import Enteties.CoefTB
import Enteties.Companys
import Enteties.TypesTransport
import Enteties.CompanyPolicyLink
import Enteties.Territories
import Shared.Validators.NothingToJust

calcOsagoPrices :: OsagoUserInfo -> IO [(Company, Float)]
calcOsagoPrices osagoUserInfo = do

  let (age, date) = nothingToJust (Views.InputOsagoData.birthDate osagoUserInfo) "getOsagoCoeffs: Ошибка получения возраста и даты"
  
  let drivingExpirience = nothingToJust (Views.InputOsagoData.drivingExpirience osagoUserInfo) "getOsagoCoeffs: Ошибка получения опыта вождения"
  
  let territorie = nothingToJust (Views.InputOsagoData.territorie osagoUserInfo) "getOsagoCoeffs: Ошибка места проживания"
  
  let autoInfo = nothingToJust (Views.InputOsagoData.autoInfo osagoUserInfo) "getOsagoCoeffs: Ошибка информации об автомобиле"
  
  let typeKs = nothingToJust (Views.InputOsagoData.typeKS osagoUserInfo) "getOsagoCoeffs: Ошибка получения срока страхования"
  
  let typeKo = nothingToJust (Views.InputOsagoData.typeKO osagoUserInfo) "getOsagoCoeffs: Ошибка получения количества водителей"
  
  let (enginePower, transportBrand, transportModel, transport, category) = autoInfo

  coefKVS <- getTypeKVS age drivingExpirience True

  let coefKT = (Enteties.Territories.coefOsago territorie)

  coefKM <- getTypeKMByPower enginePower True
  
  coefKBM <- getTypeKBMByDriverLever 3

  let summuryCoef = (Enteties.TypeKS.coefOsago typeKs) * (Enteties.TypeKO.coefOsago typeKo) * (Enteties.TypesKVS.coefOsago coefKVS) * coefKT * (Enteties.TypesKM.coefOsago coefKM) * (Enteties.TypesKBM.coefOsago coefKBM )

  let policyId = 0

  links <- getCompanyPolicyLinkByPolicy policyId

  coefsTb <- mapM (\link -> getCoefTB (companyId link) (Enteties.TypesTransport.uid category)) links

  companys <- mapM (\link -> getCompanyById (companyId link)) links

  companysWithPrice <- mapM (\(company, coef) -> return (company, (Enteties.CoefTB.value coef) * summuryCoef)) (zip companys coefsTb)

  return $ sortBy (\(_, priceX) (_, priceY) -> compare priceX priceY) companysWithPrice
