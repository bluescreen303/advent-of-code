module Main where

import Text.Printf (printf)

import Helpers (argOr)
import qualified Day_2022_09 as Lib

main :: IO ()
main = do
    filePath <- argOr "2022-09-example.txt"
    contents <- readFile filePath

    printf "the tail visited %d locations" (Lib.main contents)
