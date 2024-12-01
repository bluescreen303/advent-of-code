module Main where

import Text.Printf (printf)

import Helpers (argOr)
import qualified Year2022.Day03 as Lib


main :: IO ()
main = do
    filePath <- argOr 2022 "03-example.txt"
    contents <- readFile filePath

    let score = Lib.main contents
    printf "total priority is %d\n\n" score

    let badgeScore = Lib.main2 contents
    printf "total badgeScore priority is %d\n\n" badgeScore
