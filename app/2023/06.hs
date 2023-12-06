module Main where

import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

import Helpers (argOr)
import qualified Year2023.Day06 as Lib


main :: IO ()
main = do
    filePath <- argOr 2023 "06-example.txt"
    contents <- readFile filePath

    case Lib.main False contents of
        Left err     -> hPutStrLn stderr $ "could not parse input: " ++ show err
        Right result -> printf "product of number of ways to beat the record: %d\n\n" result

    case Lib.main True contents of
        Left err     -> hPutStrLn stderr $ "could not parse input: " ++ show err
        Right result -> printf "number of ways to beat the record for the longer race: %d\n\n" result
