module Main where

import Control.Monad (forM_)
import Text.Printf (printf)

import Helpers (argOr)
import qualified Year2022.Day06 as Lib

main :: IO ()
main = do
    filePath <- argOr 2022 "06-example.txt"
    contents <- readFile filePath

    forM_ (Lib.main contents) $ \(stream, marker) -> do
        printf "%s: first marker after character %d\n" stream marker
