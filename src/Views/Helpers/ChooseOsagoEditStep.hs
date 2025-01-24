module Views.Helpers.ChooseOsagoEditStep (chooseOsagoEditStep) where

import Shared.Inputs.ChooseData
import Shared.Logs.LogData

chooseOsagoEditStep :: String -> IO Int
chooseOsagoEditStep infoMessage = do
  let editPunkts = ["Продолжить", "Исправить дату рождения и опыт вождения", "Исправить информацию об автомобиле", "Исправить регион и место проживания", "Исправить срок страхования", "Исправить количество водителей"]
  editPunkt <- chooseData editPunkts (\array -> generateLogData array (\x -> x)) "Выберите пункт для дальнейшего действия: " infoMessage

  case editPunkt of 
    1 -> return (-1)
    _ -> return (editPunkt - 1)
