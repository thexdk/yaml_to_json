{-# LANGUAGE OverloadedStrings #-}

module YamlToJson
    ( toInternalRepr, to_json_str
    ) where

import qualified Data.Text as T
import Text.Read (readMaybe)

-- converts yaml object representation into json object representation 
to_json_str :: String -> String
to_json_str s = to_JSON (toInternalRepr 0 (lines s))

type Key = String
type ErrMsg = String
type Level = Int
data GatherErr = GatherErr deriving Show

-- value types
data YType = YSimple | YLst | YObj | YEmpty deriving Show
instance Eq YType where
    YSimple == YSimple = True
    YLst == YLst = True
    YObj == YObj = True
    _ == _ = False

-- Internal Representation of yaml object
data InternalRepr = IntRepr Level [(Key, (YType, Either GatherErr [String]))] deriving Show

-- from Internal Repsentation to string representation of json object 
to_JSON :: Either ErrMsg InternalRepr -> String
to_JSON (Left msg) = msg
to_JSON (Right (IntRepr _ [])) = ""
to_JSON (Right (IntRepr lvl [x])) = (generate_wspaces lvl) ++ "{\n" ++ (proc_kv lvl x) ++ (generate_wspaces lvl) ++ "}\n" 
to_JSON (Right (IntRepr lvl (x:xs))) = case (is_corr_repr (x:xs)) of
                                       True -> (generate_wspaces lvl) ++ "{\n" ++ (proc_kv lvl x) ++ ",\n" ++ (proc_rest lvl xs) 
                                               ++ (generate_wspaces lvl) ++ "}"
                                       _ -> "Error!"

-- this function takes well-formed key-value pair and transforms it into string
proc_kv :: Level -> (Key, (YType, Either GatherErr [String])) -> String
proc_kv _ (_, (_, Left _)) = "" -- this pattern doesnt't actually fit 
proc_kv _ (_, (YSimple, Right [])) = "" -- this pattern doesn't actually fit
proc_kv lvl (key, (YSimple, Right (value:_))) = (generate_wspaces (lvl + 1)) ++ "\"" ++ key ++ "\"" ++ ": " ++ (proc_simple value)
proc_kv lvl (key, (YObj, Right values)) = (generate_wspaces (lvl + 1)) ++ "\"" ++ key ++ "\"" ++ ": " ++
                                                (dropWhile (\el -> el == ' ') (to_JSON (toInternalRepr (lvl + 1) values)))
proc_kv lvl (key, (YLst, Right values)) = (generate_wspaces (lvl + 1)) ++ "\"" ++ key ++ "\"" ++ ": " ++
                                                "[\n" ++ (proc_lst lvl values) ++ (generate_wspaces (lvl + 1)) ++ "]"
proc_kv lvl (key, (YEmpty, _)) = (generate_wspaces (lvl + 1)) ++ "\"" ++ key ++ "\"" ++ ": " ++ "null"



-- processes the rest of key-value pairs 
proc_rest :: Level -> [(Key, (YType, Either GatherErr [String]))] -> String 
proc_rest _ [] = "" 
proc_rest lvl (x:xs) = (proc_kv lvl x) ++ ",\n" ++ (proc_rest lvl xs) 

-- transforms list of strings corresponding to YLst value into string (json styled)
proc_lst :: Level -> [String] -> String
proc_lst _ [] = ""
proc_lst lvl [x] = (generate_wspaces (lvl + 2)) ++ "\""
                   ++ (dropWhile (\el -> el == ' ') (tail x)) ++ "\"" ++ "\n"
proc_lst lvl (x:xs) = case (head x : (dropWhile (\el -> el == ' ') (tail x))) of 
                      ('-':"") -> (proc_obj_or_empty (lvl + 1) (gatherLst_cvalue xs)) 
                                      ++ (generate_wspaces lvl) ++ ",\n" ++ (proc_lst lvl xs)
                      ('-':_) -> (generate_wspaces (lvl + 2)) ++ "\""
                                     ++ (dropWhile (\el -> el == ' ') (tail x)) ++ "\"" ++ ",\n" ++ (proc_lst lvl xs)
                      (' ':_) -> (proc_lst lvl xs)
                      _ -> ""

-- transforms list of strings corresponding to YObj value into string (json styled)
-- if the list is empty -> null value
proc_obj_or_empty :: Level -> [String] -> String
proc_obj_or_empty lvl [] = (generate_wspaces (lvl + 1)) ++ "null"
proc_obj_or_empty lvl lst = (to_JSON (toInternalRepr (lvl + 1) lst)) 

-- transforms list of input file strings into Internal Representation 
toInternalRepr :: Level -> [String] -> Either ErrMsg InternalRepr
toInternalRepr lvl [] = Right (IntRepr lvl [])
toInternalRepr lvl (x:xs) = case (parseStr x) of
                            (key, value) | key == "" -> Left "Something extraneous!"
                                         | (head key) == ' ' -> Left "Bad Indentation!"
                                         | value == "" ->
                                               check_concat (IntRepr lvl [(key, (gatherComplex xs))])
                                                   (toInternalRepr lvl (skipComplex xs))
                                         | otherwise -> check_concat (IntRepr lvl [(key, (YSimple, Right [value]))]) 
                                                            (toInternalRepr lvl xs)


-- parseStr "key: cool story" -> ("key", "cool story")
parseStr :: String -> (String, String)
parseStr s = case (T.breakOnAll ":" (T.pack s)) of
                [] -> ("", "")
                ((f, st):_) -> ((T.unpack f), dropWhile (\el -> el == ' ') (tail (T.unpack st)))


-- gathers strings corresponding to nested fields into list
gatherComplex :: [String] -> (YType, Either GatherErr [String])
gatherComplex [] = (YEmpty, Right [])
gatherComplex (x:xs) = case (head x) of
                           ' ' -> (YObj, gatherObj (x:xs))
                           '-' -> (YLst, gatherLst (x:xs))
                           _ -> (YEmpty, Right [])

-- indents correspond to the nesting level 
data Indent = Nest | Same | Wrong deriving Show 

-- checks if the indent for directly nested object is correct
check_indent :: String -> Indent 
check_indent "" = Same
check_indent " " = Same
check_indent [_] = Wrong
check_indent (x:y:_) | (x == ' ') && (y == ' ') = Nest
                      | (x == ' ') && (y /= ' ') = Wrong  
                      | otherwise = Same  

instance Eq Indent where
    Nest == Nest = True
    Same == Same = True
    Wrong == Wrong = True
    _ == _ = False


-- wrapper
gather_proc :: String -> Either GatherErr [String] -> Either GatherErr [String]
gather_proc _ (Left _) = Left GatherErr
gather_proc x (Right xs) = Right (x:xs)


-- these functions gather strings of complex values into single lists

gatherObj :: [String] ->  Either GatherErr [String]
gatherObj [] = Right []
gatherObj (x:xs) | check_indent x == Nest = gather_proc (drop 2 x) (gatherObj xs)
                 | check_indent x == Wrong = Left GatherErr 
                 | otherwise = Right []


gatherLst :: [String] -> Either GatherErr [String]
gatherLst [] = Right []
gatherLst (x:xs) = case (x !! 0) of
                   '-' -> gather_proc x (gatherLst xs)
                   ' ' -> gather_proc x (gatherLst xs)
                   _ -> Right [] 



-- skips strings of complex values and returns the rest of strings 
skipComplex :: [String] -> [String]
skipComplex [] = []
skipComplex (x:xs) | x !! 0 == ' ' || x !! 0 == '-' = skipComplex xs
                   | otherwise = (x : xs) 

-- concats two well-formed Internal Represenations or returns error
check_concat :: InternalRepr -> Either ErrMsg InternalRepr -> Either ErrMsg InternalRepr
check_concat (IntRepr _ [(_, (_, Left _))]) _ = Left "Bad Indentation!"
check_concat _ (Left msg) = Left msg
check_concat (IntRepr lvl lst1) (Right (IntRepr _ lst2)) = Right (IntRepr lvl (lst1 ++ lst2)) 

-- checks if all pairs are correct
is_corr_repr :: [(Key, (YType, Either GatherErr [String]))] -> Bool
is_corr_repr [] = True
is_corr_repr ((_, (_, Left _)):_) = False
is_corr_repr ((_, (_, Right _)):xs) = is_corr_repr xs


-- checks if string can be converted into number 
isFloat :: String -> Bool
isFloat s = case readMaybe s :: Maybe Float of
            Just _ -> True
            Nothing -> False

-- checks if string represents bool value 
isLit :: String -> Bool
isLit s = case s of
          "True" -> True
          "true" -> True
          "False" -> True
          "false" -> True
          "null" -> True
          _ -> False 

-- generates whitespaces 
generate_wspaces :: Level -> String 
generate_wspaces 0 = "" 
generate_wspaces lvl = "  " ++ (generate_wspaces (lvl - 1)) 

-- gathers complex values of YLst
gatherLst_cvalue :: [String] -> [String]
gatherLst_cvalue [] = []
gatherLst_cvalue (x:xs) = case (head x) of 
                          ' ' -> ((drop 2 x) : gatherLst_cvalue xs)
                          _ -> []

-- processes YSimple values 
proc_simple :: String -> String
proc_simple s = case (isFloat s, isLit s) of
                      (True, _) -> s
                      (_, True) -> T.unpack (T.toLower (T.pack s))
                      _ -> add_quotes s

add_quotes :: String -> String
add_quotes s = case (head s, last s) of
               ('\"', '\"') -> s
               _ -> "\"" ++ s ++ "\""
