module Main where

import Text.Printf (printf)

import Helpers (argOr)
import qualified Year2023.Day01 as Lib


main :: IO ()
main = do
    filePath <- argOr 2023 "01-example.txt"
    contents <- readFile filePath

    let calibrations = Lib.main False contents
    printf "sum of calibrations: %d\n\n" calibrations

    filePath' <- argOr 2023 "01-example2.txt"
    contents' <- readFile filePath'

    let calibrations2 = Lib.main True contents'
    printf "sum of calibrations (including words): %d\n\n" calibrations2
