module YamlToJson
    ( to_json_str
    ) where

import qualified Data.Text as T


to_json_str s = toJSON (toInternalRepr 0 (lines s))

type Key = String
type ErrMsg = String
type Level = Int
data GatherErr = GatherErr

data YType = YSimple | YLst | YObj | YEmpty
instance Eq YType where
    YSimple == YSimple = True
    YLst == YLst = True
    YObj == YObj = True
    _ == _ = False


data InternalRepr = IntRepr Level [(Key, (YType, Either GatherErr [String]))]

to_JSON :: Either ErrMsg InternalRepr -> String

toInternalRepr :: Level -> [String] -> Either ErrMsg InternalRepr
toInternalRepr lvl [] = Right InternalRepr lvl []
toInternalRepr lvl (x:xs) = case (parseStr x) of
                            (key, value) | key == "" -> Left "Error!"
                                         | value == "" ->
                                               check_concat (IntRepr lvl [(key, GatherComplex lvl xs)]) (toInternalRepr lvl (SkipComplex lvl xs))
                                         | otherwise = check_concat (IntRepr lvl [(key, (YSimple, [value]))]) (toInternalRepr lvl xs)

-- Если value -- пустая строка, то на следующих строках пытаемся определять тип значения и собрать строки
-- Если обе строки непусты, то на данном шаге получаем внутреннее представление для пары ключ-значение и соединяем его с внутренним представлением оставшейся части yaml-объекта


-- parseStr "key: cool story" -> ("key", "cool story")
parseStr :: String -> (String, String)
parseStr s = case (T.breakOnAll ":" (T.pack s)) of
                [] -> ("", "")
                ((f, s):_) -> ((T.unpack f), dropWhile (\el -> el == ' ') (tail (T.unpack s)))

gatherComplex :: Level -> [String] -> (YType, [String])
gatherComplex lvl [] = (YEmpty, []) -- пустое значение
gatherComplex lvl (x:xs) = case (head x) of
                           ' ' -> (YObj, gatherObj (lvl + 1) (x:xs))
                           '-' -> (YLst, gatherLst (lvl + 1) (x:xs))
                           _ -> (YEmpty, [])

gatherObj :: Level -> [String] ->  Either GatherErr [String]
gatherObj lvl 

-- если видим пробел, то воспринимаем как вложенный объект
-- если видим черту, то как массив
-- относим строки к значению, пока не увидим строку, где в начале не пробел и не черта
-- будем для идентификации вложенности объекта использовать два пробела!


-- ParseString парсит строку и возвращает ключ-значение. Если ключ пустой, то ошибка. Значение -> значение имеет сложный тип
-- GatherComplex возвращает тип сложного значения и соответствующий список строк
-- SkipComplex пропускает все строки, относящиеся к сложному значению и возвращает оставшиеся строки 
-- check_concat работает сливает 2 внутренних представления одного уровня в одно, если нет ошибки -- иначе возвращает ошибку




