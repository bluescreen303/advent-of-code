module Main where

import Text.Printf (printf)

import Helpers (argOr)
import qualified Year2023.Day11 as Lib


main :: IO ()
main = do
    filePath <- argOr 2023 "11-example.txt"
    contents <- readFile filePath

    printf "sum of shortest paths between all galaxies (scale factor       2): %d\n\n" $ Lib.main False contents
    printf "sum of shortest paths between all galaxies (scale factor 1000000): %d\n\n" $ Lib.main True  contents