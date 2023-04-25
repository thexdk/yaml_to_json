{-# LANGUAGE OverloadedStrings #-}

module YamlToJson
    ( toInternalRepr, to_JSON
    ) where

import qualified Data.Text as T
import Text.Read (readMaybe)


-- to_json_str s = toJSON (toInternalRepr 0 (lines s))

type Key = String
type ErrMsg = String
type Level = Int
data GatherErr = GatherErr deriving Show

data YType = YSimple | YLst | YObj | YEmpty deriving Show
instance Eq YType where
    YSimple == YSimple = True
    YLst == YLst = True
    YObj == YObj = True
    _ == _ = False


data InternalRepr = IntRepr Level [(Key, (YType, Either GatherErr [String]))] deriving Show

to_JSON :: Either ErrMsg InternalRepr -> String
to_JSON (Left msg) = msg
to_JSON (Right (IntRepr lvl [])) = ""
to_JSON (Right (IntRepr lvl [x])) = (generate_wspaces lvl) ++ "{\n" ++ (proc_kv lvl x) ++ (generate_wspaces lvl) ++ "}\n" 
to_JSON (Right (IntRepr lvl (x:xs))) = (generate_wspaces lvl) ++ "{\n" ++ (proc_kv lvl x) ++ (proc_rest lvl xs) 
                                           ++ (generate_wspaces lvl) ++ "}\n"

proc_kv :: Level -> (Key, (YType, Either GatherErr [String])) -> String 
proc_kv lvl (key, (YSimple, Right [value])) = (generate_wspaces (lvl + 1)) ++ "\"" ++ key ++ "\"" ++ ": " ++ value ++ "\n"
proc_kv lvl (key, (YObj, Right values)) = (generate_wspaces (lvl + 1)) ++ "\"" ++ key ++ "\"" ++ ": " ++
                                                (to_JSON (toInternalRepr (lvl + 1) values))
proc_kv lvl (key, (YLst, Right values)) = (generate_wspaces (lvl + 1)) ++ "\"" ++ key ++ "\"" ++ ": " ++
                                                "[\n" ++ (proc_lst lvl values) ++ (generate_wspaces (lvl + 1)) ++ "]\n"
proc_kv _ (_, _) = "" 

-- написать обертку!
-- запятые!

proc_rest :: Level -> [(Key, (YType, Either GatherErr [String]))] -> String 
proc_rest _ [] = "" 
proc_rest lvl (x:xs) = (proc_kv lvl x) ++ (proc_rest lvl xs) 

proc_lst :: Level -> [String] -> String
proc_lst _ [] = ""
proc_lst lvl [x] = (generate_wspaces (lvl + 2)) ++ "\""
                   ++ (dropWhile (\el -> el == ' ') (tail x)) ++ "\"" ++ "\n"
proc_lst lvl (x:xs) = (generate_wspaces (lvl + 2)) ++ "\""
                   ++ (dropWhile (\el -> el == ' ') (tail x)) ++ "\"" ++ ",\n" ++ (proc_lst lvl xs)
 

toInternalRepr :: Level -> [String] -> Either ErrMsg InternalRepr
toInternalRepr lvl [] = Right (IntRepr lvl [])
toInternalRepr lvl (x:xs) = case (parseStr x) of
                            (key, value) | key == "" -> Left "Error!"
                                         | value == "" ->
                                               check_concat (IntRepr lvl [(key, (gatherComplex xs))])
                                                   (toInternalRepr lvl (skipComplex xs))
                                         | otherwise -> check_concat (IntRepr lvl [(key, (YSimple, Right [value]))]) 
                                                            (toInternalRepr lvl xs)

-- Если value -- пустая строка, то на следующих строках пытаемся определять тип значения и собрать строки
-- Если обе строки непусты, то на данном шаге получаем внутреннее представление для пары ключ-значение
-- и соединяем его с внутренним представлением оставшейся части yaml-объекта


-- parseStr "key: cool story" -> ("key", "cool story")
parseStr :: String -> (String, String)
parseStr s = case (T.breakOnAll ":" (T.pack s)) of
                [] -> ("", "")
                ((f, st):_) -> ((T.unpack f), dropWhile (\el -> el == ' ') (tail (T.unpack st)))

gatherComplex :: [String] -> (YType, Either GatherErr [String])
gatherComplex [] = (YEmpty, Right []) -- пустое значение
gatherComplex (x:xs) = case (head x) of
                           ' ' -> (YObj, gatherObj (x:xs))
                           '-' -> (YLst, gatherLst (x:xs))
                           _ -> (YEmpty, Right [])

data Ident = Nest | Same | Wrong deriving Show 
check_ident :: String -> Ident 
check_ident "" = Same
check_ident " " = Same
check_ident [_] = Wrong
check_ident (x:y:_) | (x == ' ') && (y == ' ') = Nest  
                    | (x == ' ') && (y /= ' ') = Wrong  
                    | otherwise = Same  

instance Eq Ident where
    Nest == Nest = True
    Same == Same = True
    Wrong == Wrong = True
    _ == _ = False


 
gather_proc :: String -> Either GatherErr [String] -> Either GatherErr [String]
gather_proc _ (Left _) = Left GatherErr
gather_proc x (Right xs) = Right (x:xs)


gatherObj :: [String] ->  Either GatherErr [String]
gatherObj [] = Right []
gatherObj (x:xs) | check_ident x == Nest = gather_proc (drop 2 x) (gatherObj xs)
                 | check_ident x == Wrong = Left GatherErr 
                 | otherwise = Right []


gatherLst :: [String] -> Either GatherErr [String]
gatherLst [] = Right []
gatherLst (x:xs) = case (x !! 0) of
                   '-' -> gather_proc x (gatherLst xs)
                   ' ' -> gather_proc x (gatherLst xs)
                   _ -> Right [] 



-- наличие пробела или черты свидетельствует о значении сложного типа
skipComplex :: [String] -> [String]
skipComplex [] = []
skipComplex (x:xs) | x !! 0 == ' ' || x !! 0 == '-' = skipComplex xs
                   | otherwise = (x : xs) 


check_concat :: InternalRepr -> Either ErrMsg InternalRepr -> Either ErrMsg InternalRepr
check_concat (IntRepr _ [(_, (_, Left _))]) _ = Left "Error!"
check_concat _ (Left _) = Left "Error!"
check_concat (IntRepr lvl lst1) (Right (IntRepr _ lst2)) = Right (IntRepr lvl (lst1 ++ lst2)) 


isFloat :: String -> Bool
isFloat s = case readMaybe s :: Maybe Float of
            Just _ -> True
            Nothing -> False

generate_wspaces :: Level -> String 
generate_wspaces 0 = "" 
generate_wspaces lvl = "  " ++ (generate_wspaces (lvl - 1)) 



-- если видим пробел, то воспринимаем как вложенный объект
-- если видим черту, то как массив
-- относим строки к значению, пока не увидим строку, где в начале не пробел и не черта
-- будем для идентификации вложенности объекта использовать два пробела!


-- ParseString парсит строку и возвращает ключ-значение. Если ключ пустой, то ошибка. Значение -> значение имеет сложный тип
-- GatherComplex возвращает тип сложного значения и соответствующий список строк
-- SkipComplex пропускает все строки, относящиеся к сложному значению и возвращает оставшиеся строки 
-- check_concat сливает 2 внутренних представления одного уровня в одно, если нет ошибки -- иначе возвращает ошибку




