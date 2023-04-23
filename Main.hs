module Main (main) where

import YamlToJson
import System.IO

main :: IO ()
main = do
    fileHandle <- openFile "jsoned" WriteMode
    yaml_object <- readFile "test"
    hPrint fileHandle (to_json_str yaml_object)
    hClose fileHandle
    

