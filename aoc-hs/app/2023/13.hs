module Main where

import Text.Printf (printf)

import Helpers (argOr)
import qualified Year2023.Day13 as Lib


main :: IO ()
main = do
    filePath <- argOr 2023 "13-example.txt"
    contents <- readFile filePath
    printf "Note summarization: %d\n\n" (Lib.main False contents)
    printf "Note summarization (correcting 1 smudge): %d\n\n" (Lib.main True  contents)
