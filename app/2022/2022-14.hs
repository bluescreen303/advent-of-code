module Main where

import Helpers (argOr)
import qualified Year2022.Day14 as Lib

main :: IO ()
main = do
    filePath <- argOr 2022 "14-example.txt"
    contents <- readFile filePath

    print $ Lib.main contents
