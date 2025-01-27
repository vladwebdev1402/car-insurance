module Views.UserInfo (UserInfo(..)) where

import Enteties.Regions
import Enteties.Territories
import Enteties.Additional
import Enteties.TypeKS
import Enteties.TypeKO
import Enteties.TransportBrands 
import Enteties.TransportModels 
import Enteties.Transports
import Enteties.TypesTransport
import Enteties.TransportCertificate
import Enteties.PolicyServices
import Enteties.Deductibles
import Views.Helpers.GetAutoInfo

data UserInfo = UserInfo {
  birthDate :: Maybe (Int, String),
  drivingExpirience :: Maybe Int,
  autoInfo :: Maybe (Int, Maybe TransportBrand, Maybe TransportModel, Maybe Transport, TypeTransport, Maybe TransportCertificate),
  region :: Maybe Region,
  territorie :: Maybe Territorie,
  typeKS :: Maybe TypeKS,
  typeKO :: Maybe TypeKO,
  deductible :: Maybe Deductible,
  additional :: Maybe Additional,
  policyServices :: Maybe [PolicyService]
} deriving (Read, Show)

nullUserInfo :: UserInfo
nullUserInfo = UserInfo {Views.UserInfo.birthDate = Nothing, 
                             Views.UserInfo.drivingExpirience = Nothing,
                             Views.UserInfo.autoInfo = Nothing, 
                             Views.UserInfo.region = Nothing, 
                             Views.UserInfo.territorie = Nothing, 
                             Views.UserInfo.typeKS = Nothing, 
                             Views.UserInfo.typeKO = Nothing,
                             Views.UserInfo.deductible = Nothing,
                             Views.UserInfo.policyServices = Nothing,
                             Views.UserInfo.additional = Nothing
                             }

getUserInfoString :: UserInfo -> String
getUserInfoString userInfo = 
    (case birthDate userInfo of
            Nothing -> ""
            Just (age, birthDate) -> "Возраст: " ++ show age ++ " лет\nДата рождения: " ++ birthDate ++ "\n") ++
    (case drivingExpirience userInfo of
            Nothing -> ""
            Just expirience -> "Стаж вождения: " ++ show expirience ++ " лет\n") ++
    (case autoInfo userInfo of
            Nothing -> ""
            Just (a, b, c, d, e, f) -> getAutoInfo a b c d e f ++ "\n") ++
    (case region userInfo of
            Nothing -> ""
            Just region -> "Регион: " ++ (Enteties.Regions.name region) ++ "\n") ++
    (case territorie userInfo of
            Nothing -> ""
            Just territorie -> "Территория: " ++ (Enteties.Territories.name territorie) ++ "\n") ++
    (case typeKS userInfo of
            Nothing -> ""
            Just typeKS -> "Срок страхования: " ++ (Enteties.TypeKS.description typeKS) ++ "\n") ++
    (case typeKO userInfo of
            Nothing -> ""
            Just typeKO -> "Количество водителей: " ++  (Enteties.TypeKO.description typeKO)++ "\n") ++
    (case deductible userInfo of
            Nothing -> ""
            Just deductible -> "Размер франшизы: " ++ (show (Enteties.Deductibles.sumDeductible deductible) )++ "\n") ++
    (case policyServices userInfo of
            Nothing -> ""
            Just services -> (concat (map (\x -> (Enteties.PolicyServices.name x) ++ ", ") services))) ++
    (case additional userInfo of
            Nothing -> ""
            Just additional -> "Дополнительная сумма страхования: " ++ (show (Enteties.Additional.value additional)))
 
    
   
