module Main (main) where

import YamlToJson
import System.IO
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    args_proc args


args_proc :: [String] -> IO ()
args_proc args = case args of
    [] -> putStrLn "No input file given"
    [x] -> files_proc x "jsoned.txt"
    (x:y:_) -> files_proc x y

files_proc :: String -> String -> IO ()
files_proc fin fout = do
                          yaml_object <- readFile fin
                          fileHandle <- openFile fout WriteMode
                          hPutStr fileHandle (to_json_str yaml_object)
                          putStrLn ("The output is in file " ++ fout)
                          hClose fileHandle




