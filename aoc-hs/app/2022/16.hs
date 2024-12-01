module Main where

import Helpers (argOr)
import qualified Year2022.Day16 as Lib

main :: IO ()
main = do
    filePath <- argOr 2022 "16-example.txt"
    contents <- readFile filePath

    print $ Lib.main contents
