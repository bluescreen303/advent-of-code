module Main where

import Helpers (argOr')
import qualified Day_2022_15 as Lib

main :: IO ()
main = do
    (filePath, rest) <- argOr' 1 "2022-15-example.txt"
    let mline = case rest of
                  []    -> Nothing
                  (x:_) -> Just (read x)
    contents <- readFile filePath

    print $ Lib.main mline contents
