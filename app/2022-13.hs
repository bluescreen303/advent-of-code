module Main where

import Helpers (argOr)
import qualified Day_2022_13 as Lib

main :: IO ()
main = do
    filePath <- argOr "2022-13-example.txt"
    contents <- readFile filePath

    print $ Lib.main contents
    print $ Lib.main2 contents
