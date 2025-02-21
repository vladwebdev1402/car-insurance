module Main (main) where

import Views.RegistrationUser
import System.Process (callCommand)
import Shared.Validators.ValidateNumberRangeInput
import Views.InformationInsurance
import Views.CalcPriceInsurance
import Views.RegistrationInsurance

main :: IO ()
main = do
  callCommand "clear" 
  putStrLn "Выберите пункт меню:\n1. Рассчитать стоимость страхования\n2. Оформить страховой полис\n3. Получить информацию о страховых полисах\n4. Зарегестрироваться в системе\n5. Выход"
  input <- getLine
  case validateNumberRangeInput 1 5 input of
    Just choice -> do 
      callCommand "clear" 
      executeChoice choice
    Nothing -> do
      callCommand "clear" 
      putStrLn "Ошибка: введите число от 1 до 3."
      main

executeChoice :: Int -> IO ()
executeChoice 1 = do 
  calcPriceInsurance
  main

executeChoice 2 = do 
  registrationInsurance
  main
  
executeChoice 3 = do 
  informationInsurance ""
  main
  
executeChoice 4 = do 
  registrationUser
  main

executeChoice 5 = do 
  putStrLn "До свидания"