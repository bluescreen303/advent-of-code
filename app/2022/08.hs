module Main where

import Text.Printf (printf)

import Helpers (argOr)
import qualified Year2022.Day08 as Lib

main :: IO ()
main = do
    filePath <- argOr 2022 "08-example.txt"
    contents <- readFile filePath

    case Lib.main contents of
        Nothing      -> error "invalid grid"
        Just (r, s)  -> printf "visible: %d\nbest scenic score: %d\n" r s
