module Main where

import Text.Printf (printf)

import Helpers (argOr)
import qualified Day_2022_01 as Lib


main :: IO ()
main = do
    filePath <- argOr "2022-01-example.txt"
    contents <- readFile filePath

    let (elf, calories) = Lib.main contents
    printf "elf %d is carrying %d calories\n\n" (elf + 1) calories

    let top3Calories = Lib.main2 contents
    printf "the top 3 elves are carrying %d calories\n" top3Calories
