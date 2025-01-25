module Views.Helpers.ChooseOsagoEditStep (chooseOsagoEditStep) where

import Shared.Inputs.ChooseData
import Shared.Logs.LogData

chooseOsagoEditStep :: Bool -> String -> IO Int
chooseOsagoEditStep False infoMessage = do
  let editPunkts = ["Продолжить", "Изменить дату рождения и опыт вождения", "Изменить информацию об автомобиле", "Изменить регион и место проживания", "Изменить срок страхования", "Изменить количество водителей"]
  editPunkt <- chooseData editPunkts (\array -> generateLogData array (\x -> x)) "Выберите пункт для дальнейшего действия: " infoMessage

  case editPunkt of 
    1 -> return (-1)
    _ -> return (editPunkt - 1)
  
chooseOsagoEditStep True infoMessage = do
  let editPunkts = ["Продолжить", "Изменить регистрационный номер", "Изменить регион и место проживания", "Изменить срок страхования", "Изменить количество водителей"]
  editPunkt <- chooseData editPunkts (\array -> generateLogData array (\x -> x)) "Выберите пункт для дальнейшего действия: " infoMessage

  case editPunkt of 
    1 -> return (-1)
    _ -> return (editPunkt - 1)
