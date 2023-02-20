module Main where

import Helpers (argOr')
import qualified Day_2022_15 as Lib

main :: IO ()
main = do
    (filePath, rest) <- argOr' 1 "2022-15-example.txt"
    let m = case rest of
                []    -> 20
                (x:_) -> read x
    contents <- readFile filePath

    print $ Lib.main m contents
