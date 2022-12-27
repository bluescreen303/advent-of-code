module Main where

import Text.Printf (printf)

import Helpers (argOr)
import qualified Day_2022_02 as Lib


main :: IO ()
main = do
    filePath <- argOr "2022-02-example.txt"
    contents <- readFile filePath

    let score = Lib.main contents
    printf "score is %d\n\n" score
