module Main (main) where

import System.Process (callCommand)
import Shared.Validators.ValidateNumberRangeInput
import Views.InformationInsurance
import Views.CalcPriceInsurance
import Views.RegistrationInsurance

main :: IO ()
main = do
  callCommand "cls" 
  putStrLn "Выберите пункт меню:\n1. Рассчитать стоимость страховки\n2. Оформить страховку\n3. Получить информацию о страховках"
  input <- getLine
  case validateNumberRangeInput 1 3 input of
    Just choice -> do 
      callCommand "cls" 
      executeChoice choice
    Nothing -> do
      callCommand "cls" 
      putStrLn "Ошибка: введите число от 1 до 3."
      main

executeChoice :: Int -> IO ()
executeChoice 1 = calcPriceInsurance
executeChoice 2 = registrationInsurance
executeChoice 3 = informationInsurance
executeChoice _ = return () 