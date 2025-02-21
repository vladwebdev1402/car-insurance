module Views.Helpers.ChoosePolicy (choosePolicy) where

import System.Process (callCommand)
import Entities.Policies
import Entities.PolicyTypes
import Modules.FullPolicyInfo
import Shared.Logs.LogData
import Shared.Logs.Console
import Shared.Validators.ValidateStringNumber
import Data.Char (toLower)

filterPolicies :: [FullPolicyInfo] -> String -> String -> [FullPolicyInfo]
filterPolicies fullInfos status policyType =
    filter (\item ->
        (status == "" || Entities.Policies.status (Modules.FullPolicyInfo.policy item) == status) &&
        (policyType == "" || (map toLower (Entities.PolicyTypes.name (Modules.FullPolicyInfo.policyType item))) == policyType)
    ) fullInfos

choosePolicy :: [FullPolicyInfo] -> Int -> String -> String -> IO (Maybe FullPolicyInfo)
choosePolicy fullInfos page status policyType = do
    let pageSize = 3 
    
    let (minIdx, maxIdx) = ((page * pageSize), (page * pageSize + (pageSize -1)))

    let filteredPolicies = filterPolicies fullInfos status policyType

    let takedPolicies = take (maxIdx - minIdx + 1) (drop minIdx filteredPolicies)

    putStrLn "\nВыберите страховой договор, для отмены\nВведите 'активен' или 'не активен' для фильтрации страховых договоров по статусам\nВведите вид страхования для фильтрации (ОСАГО, КАСКО, ДСАГО)\nВведите 'сбросить' для сброса фильтров\nВведите 'вперёд' или 'назал' для смены страницы"
    putStrLn ("\nТекущая страница: " ++ (show (page + 1)) ++ "\n")
    generateLogData takedPolicies (\item -> getFullInfoForPolicyString item)
    input <- getLine
    callCommand "clear"

    if (map toLower input) `elem` ["осаго", "дсаго", "каско"] then choosePolicy fullInfos 0 status input
    else if (map toLower input) `elem` ["активен", "не активен"] then choosePolicy fullInfos 0 (policyStatusEngTranslate input) policyType
    else case input of 
        "выход" -> return Nothing   
        "сбросить" -> choosePolicy fullInfos 0 "" ""   
        "назад" -> choosePolicy fullInfos (max 0 (page - 1)) status policyType   
        "вперёд" -> if (page + 1) * pageSize >= length filteredPolicies
                    then choosePolicy fullInfos page status policyType 
                    else choosePolicy fullInfos (page + 1) status policyType   
        num -> if validateStringNumber num 1 (length filteredPolicies) then do
                    let policy = filteredPolicies !! ((read num) - 1)
                    if (Entities.Policies.status (Modules.FullPolicyInfo.policy policy)) == "deactive" then do 
                        callCommand "clear"
                        putStrLn "Полис уже не активен, выберите другой"
                        choosePolicy fullInfos page status policyType
                    else return (Just policy)
            else do
                callCommand "clear"
                consoleError "Неверно введённые данные"
                choosePolicy fullInfos page status policyType