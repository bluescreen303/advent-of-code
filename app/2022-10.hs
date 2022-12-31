module Main where

import Text.Printf (printf)

import Helpers (argOr)
import qualified Day_2022_10 as Lib

main :: IO ()
main = do
    filePath <- argOr "2022-10-example.txt"
    contents <- readFile filePath

    printf "sum of signal strengths: %d\n\n" (Lib.main contents)
    putStr (Lib.main2 contents)
