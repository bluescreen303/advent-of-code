module Main where

import Helpers (argOr)
import qualified Day_2022_14 as Lib

main :: IO ()
main = do
    filePath <- argOr "2022-14-example.txt"
    contents <- readFile filePath

    print $ Lib.main contents
