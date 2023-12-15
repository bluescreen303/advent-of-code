module Main where

import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

import Helpers (argOr)
import qualified Year2023.Day15 as Lib


main :: IO ()
main = do
    filePath <- argOr 2023 "15-example.txt"
    contents <- readFile filePath

    case Lib.main False contents of
        Left err     -> hPutStrLn stderr $ "could not parse input: " ++ show err
        Right result -> printf "sum of the hashed results: %d\n\n" result

    case Lib.main True contents of
        Left err     -> hPutStrLn stderr $ "could not parse input: " ++ show err
        Right result -> printf "focusing power of lens configuration: %d\n\n" result
