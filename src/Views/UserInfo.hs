module Views.UserInfo (UserInfo(..), nullUserInfo) where

import Entities.Regions
import Entities.Territories
import Entities.Additional
import Entities.TypeKS
import Entities.TypeKO
import Entities.TransportBrands 
import Entities.TransportModels 
import Entities.Transports
import Entities.TypesTransport
import Entities.TransportCertificate
import Entities.PolicyServices
import Entities.Deductibles
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
            Just region -> "Регион: " ++ (Entities.Regions.name region) ++ "\n") ++
    (case territorie userInfo of
            Nothing -> ""
            Just territorie -> "Территория: " ++ (Entities.Territories.name territorie) ++ "\n") ++
    (case typeKS userInfo of
            Nothing -> ""
            Just typeKS -> "Срок страхования: " ++ (Entities.TypeKS.description typeKS) ++ "\n") ++
    (case typeKO userInfo of
            Nothing -> ""
            Just typeKO -> "Количество водителей: " ++  (Entities.TypeKO.description typeKO)++ "\n") ++
    (case deductible userInfo of
            Nothing -> ""
            Just deductible -> "Размер франшизы: " ++ (show (Entities.Deductibles.sumDeductible deductible) )++ "\n") ++
    (case policyServices userInfo of
            Nothing -> ""
            Just services -> (concat (map (\x -> (Entities.PolicyServices.name x) ++ ", ") services))) ++
    (case additional userInfo of
            Nothing -> ""
            Just additional -> "Дополнительная сумма страхования: " ++ (show (Entities.Additional.value additional)))
 
    
   
