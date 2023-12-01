module Main where

import Text.Printf (printf)

import Helpers (argOr)
import qualified Year2022.Day04 as Lib

main :: IO ()
main = do
    filePath <- argOr 2022 "04-example.txt"
    contents <- readFile filePath

    case Lib.main contents of
        Left err -> error $ "parse error " ++ show err
        Right result -> printf "found %d pairs with full overlap\n\n" result

    case Lib.main2 contents of
        Left err -> error $ "parse error " ++ show err
        Right result -> printf "found %d pairs with some overlap\n\n" result
