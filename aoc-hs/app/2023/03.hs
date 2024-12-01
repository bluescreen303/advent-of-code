module Main where

import Text.Printf (printf)

import Helpers (argOr)
import qualified Year2023.Day03 as Lib


main :: IO ()
main = do
    filePath <- argOr 2023 "03-example.txt"
    contents <- readFile filePath

    let result1 = Lib.main False contents
    printf "sum of all of the part numbers: %d\n\n" result1
    let result2 = Lib.main True contents
    printf "sum of all of the gear ratios: %d\n\n" result2
