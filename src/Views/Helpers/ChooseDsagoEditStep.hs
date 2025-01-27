module Views.Helpers.ChooseDsagoEditStep (chooseDsagoEditStep) where

import Shared.Inputs.ChooseData
import Shared.Logs.LogData

chooseDsagoEditStep :: Bool -> String -> IO Int
chooseDsagoEditStep False infoMessage = do
  let editPunkts = ["Продолжить", "Изменить дату рождения и опыт вождения", "Изменить информацию об автомобиле", "Изменить регион и место проживания", "Изменить срок страхования", "Изменить количество водителей", "Изменить размер дополнительной суммы"]
  editPunkt <- chooseData editPunkts (\array -> generateLogData array (\x -> x)) "Выберите пункт для дальнейшего действия: " infoMessage

  case editPunkt of 
    1 -> return (-1)
    _ -> return (editPunkt - 1)