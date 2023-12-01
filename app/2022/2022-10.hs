module Main where

import Text.Printf (printf)

import Helpers (argOr)
import qualified Year2022.Day10 as Lib

main :: IO ()
main = do
    filePath <- argOr 2022 "10-example.txt"
    contents <- readFile filePath

    printf "sum of signal strengths: %d\n\n" (Lib.main contents)
    putStr (Lib.main2 contents)
