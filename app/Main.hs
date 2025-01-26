module Main (main) where

import System.Process (callCommand)
import Shared.Validators.ValidateNumberRangeInput
import Views.InformationInsurance
import Views.CalcPriceInsurance
import Views.RegistrationInsurance

main :: IO ()
main = do
  callCommand "cls" 
  putStrLn "Выберите пункт меню:\n1. Рассчитать стоимость страхования\n2. Оформить страховой полис\n3. Получить информацию о страховых полисах\n4. Выход"
  input <- getLine
  case validateNumberRangeInput 1 4 input of
    Just choice -> do 
      callCommand "cls" 
      executeChoice choice
    Nothing -> do
      callCommand "cls" 
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
  
executeChoice _ = return () 