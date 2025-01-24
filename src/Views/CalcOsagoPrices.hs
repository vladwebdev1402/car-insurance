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

calcOsagoPrices :: OsagoUserInfo -> IO [(Company, Float)]
calcOsagoPrices osagoUserInfo = do
  (age, date) <- case Views.InputOsagoData.birthDate osagoUserInfo of
    Nothing -> error "getOsagoCoeffs: Ошибка получения возраста и даты"
    Just (age, date) -> return (age, date)

  drivingExpirience <- case Views.InputOsagoData.drivingExpirience osagoUserInfo of
    Nothing -> error "getOsagoCoeffs: Ошибка получения опыта вождения"
    Just drivingExpirience -> return drivingExpirience

  territorie <- case Views.InputOsagoData.territorie osagoUserInfo of
    Nothing -> error "getOsagoCoeffs: Ошибка места проживания"
    Just territorie -> return territorie

  autoInfo <- case Views.InputOsagoData.autoInfo osagoUserInfo of
    Nothing -> error "getOsagoCoeffs: Ошибка информации об автомобиле"
    Just autoInfo -> return autoInfo

  let (enginePower, transportBrand, transportModel, transport, category) = autoInfo

  typeKs <- case Views.InputOsagoData.typeKS osagoUserInfo of
    Nothing -> error "getOsagoCoeffs: Ошибка получения срока страхования"
    Just typeKs -> return typeKs
  
  typeKo <- case Views.InputOsagoData.typeKO osagoUserInfo of
    Nothing -> error "getOsagoCoeffs: Ошибка получения количества водителей"
    Just typeKo -> return typeKo

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
