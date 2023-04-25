module Main (main) where

import YamlToJson
import System.IO

main :: IO ()
main = do
    putStrLn "Enter file name:"
    fileName <- getLine
    fileHandle <- openFile "jsoned.txt" WriteMode
    yaml_object <- readFile fileName
    hPutStr fileHandle (to_json_str yaml_object)
    putStrLn "The output is in file \"jsoned\""
    hClose fileHandle


