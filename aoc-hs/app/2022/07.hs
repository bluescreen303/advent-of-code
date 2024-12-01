module Main where

import Text.Printf (printf)

import Helpers (argOr)
import qualified Year2022.Day07 as Lib

main :: IO ()
main = do
    filePath <- argOr 2022 "07-example.txt"
    contents <- readFile filePath

    case Lib.main contents of
        Left  err -> error $ "parse error " ++ show err
        Right r   -> printf "best dir would cleanup: %d\n" r
