module Main where

import Helpers (argOr)
import qualified Year2022.Day12 as Lib

main :: IO ()
main = do
    filePath <- argOr 2022 "12-example.txt"
    contents <- readFile filePath

    print $ Lib.main contents
    print $ Lib.mainAll contents
