module Main where

import Helpers (argOr)
import qualified Day_2022_16 as Lib

main :: IO ()
main = do
    filePath <- argOr "2022-16-example.txt"
    contents <- readFile filePath

    print $ Lib.main contents
