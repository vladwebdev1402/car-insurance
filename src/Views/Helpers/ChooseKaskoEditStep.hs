module Views.Helpers.ChooseKaskoEditStep (chooseKaskoEditStep) where

import Shared.Inputs.ChooseData
import Shared.Logs.LogData

chooseKaskoEditStep :: Bool -> String -> IO Int
chooseKaskoEditStep False infoMessage = do
  let editPunkts = ["Продолжить", "Изменить дату рождения и опыт вождения", "Изменить информацию об автомобиле", "Изменить регион и место проживания", "Изменить срок страхования", "Изменить количество водителей", "Изменить франшизу", "Изменить выбранные услуги"]
  editPunkt <- chooseData editPunkts (\array -> generateLogData array (\x -> x)) "Выберите пункт для дальнейшего действия: " infoMessage

  case editPunkt of 
    1 -> return (-1)
    _ -> return (editPunkt - 1)
