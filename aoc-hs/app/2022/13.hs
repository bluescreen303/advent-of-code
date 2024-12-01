module Main where

import Helpers (argOr)
import qualified Year2022.Day13 as Lib

main :: IO ()
main = do
    filePath <- argOr 2022 "13-example.txt"
    contents <- readFile filePath

    print $ Lib.main contents
    print $ Lib.main2 contents
