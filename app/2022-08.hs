module Main where

import Text.Printf (printf)

import Helpers (argOr)
import qualified Day_2022_08 as Lib

main :: IO ()
main = do
    filePath <- argOr "2022-08-example.txt"
    contents <- readFile filePath

    case Lib.main contents of
        Nothing      -> error "invalid grid"
        Just (r, s)  -> printf "visible: %d\nbest scenic score: %d\n" r s
