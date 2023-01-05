module Main where

import Helpers (argOr)
import qualified Day_2022_11 as Lib

main :: IO ()
main = do
    filePath <- argOr "2022-11-example.txt"
    contents <- readFile filePath

    case Lib.main 10000 contents of
        Left e  -> error $ "parse error: " ++ show e
        Right q -> print q