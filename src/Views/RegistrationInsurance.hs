module Views.RegistrationInsurance (registrationInsurance) where

import Views.InputOsagoData
import Enteties.PolicyTypes
import Modules.ChoosePolicyType

registrationInsurance :: IO ()
registrationInsurance = do
  policyType <- choosePolicyType

  case (Enteties.PolicyTypes.uid policyType) of
    0 -> registrationOsago Nothing (-1) 
    _ -> return ()

registrationOsago :: Maybe OsagoUserInfo -> Int -> IO ()
registrationOsago oldOsagoUserInfo editPunkt = do
  let osagoUserInfo = case oldOsagoUserInfo of
        Nothing -> nullOsagoUserInfo
        _ -> osagoUserInfo

  osagoUserInfo <- inputOsagoData osagoUserInfo editPunkt True ""

  case (birthDate osagoUserInfo) of
    Nothing -> return ()
    _ -> putStrLn "Регистрация ОСАГО: Функция в разработке."
  
  