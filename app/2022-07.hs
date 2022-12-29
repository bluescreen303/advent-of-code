module Main where

import Text.Printf (printf)

import Helpers (argOr)
import qualified Day_2022_07 as Lib

main :: IO ()
main = do
    filePath <- argOr "2022-07-example.txt"
    contents <- readFile filePath

    case Lib.main contents of
        Left  err -> error $ "parse error " ++ show err
        Right r   -> printf "sum of cleanup: %d\n" r
