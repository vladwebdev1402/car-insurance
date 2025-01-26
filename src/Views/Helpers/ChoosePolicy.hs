module Views.Helpers.ChoosePolicy (choosePolicy) where

import System.Process (callCommand)
import Enteties.Policies
import Enteties.PolicyTypes
import Modules.FullPolicyInfo
import Shared.Logs.LogData
import Shared.Logs.Console
import Shared.Validators.ValidateStringNumber
import Data.Char (toLower)

translateStatus :: String -> String
translateStatus "активная" = "active"
translateStatus "не активная" = "deactive"

filterPolicies :: [FullPolicyInfo] -> String -> String -> [FullPolicyInfo]
filterPolicies fullInfos status policyType =
    filter (\item ->
        (status == "" || Enteties.Policies.status (Modules.FullPolicyInfo.policy item) == status) &&
        (policyType == "" || Enteties.PolicyTypes.name (Modules.FullPolicyInfo.policyType item) == policyType)
    ) fullInfos

choosePolicy :: [FullPolicyInfo] -> Int -> String -> String -> IO (Maybe FullPolicyInfo)
choosePolicy fullInfos page status policyType = do
    let pageSize = 5 
    let (minIdx, maxIdx) = ((page * pageSize), (page * pageSize + (pageSize -1)))
    let takedPolicies = take (maxIdx - minIdx + 1) (drop minIdx fullInfos)
    let filteredPolicies = filterPolicies takedPolicies status policyType

    putStrLn "\nВыберите страховку, для отмены\nВведите 'активная' или 'не активная' для фильтрации страховки\nВведите тип страховки для фильтрации (ОСАГО, КАСКО, ДСАГО)\nВведите сбросить для сброса фильтров\nВведите вперёд, назал для смены страницы"
    putStrLn ("\nТекущая страница: " ++ (show (page + 1)) ++ "\n")
    generateLogData filteredPolicies (\item -> getFullInfoForPolicyString item)
    input <- getLine
    callCommand "cls"

    if (map toLower input) `elem` ["осаго", "дсаго", "каско"] then choosePolicy fullInfos 0 status input
    else if (map toLower input) `elem` ["активная", "не активная"] then choosePolicy fullInfos 0 (translateStatus input) policyType
    else case input of 
        "выход" -> return Nothing   
        "сбросить" -> choosePolicy fullInfos 0 "" ""   
        "назад" -> choosePolicy fullInfos (max 0 (page - 1)) status policyType   
        "вперёд" -> if (page + 1) * pageSize >= length fullInfos
                    then choosePolicy fullInfos page status policyType 
                    else choosePolicy fullInfos (page + 1) status policyType   
        num -> if validateStringNumber num 1 (length filteredPolicies) then do
                    let index = read num
                    return $ Just (filteredPolicies !! (index - 1))
            else do
                callCommand "cls"
                consoleError "Неверно введённые данные"
                choosePolicy fullInfos page status policyType