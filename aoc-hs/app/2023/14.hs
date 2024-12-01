module Main where

import Text.Printf (printf)

import Helpers (argOr)
import qualified Year2023.Day14 as Lib


main :: IO ()
main = do
    filePath <- argOr 2023 "14-example.txt"
    contents <- readFile filePath

    printf "total load on north support beams: %d\n\n" $ Lib.main False contents
    printf "total load on north support beams (after 1_000_000_000 platform cycles): %d\n\n" $ Lib.main True  contents
