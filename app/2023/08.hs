module Main where

import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

import Helpers (argOr)
import qualified Year2023.Day08 as Lib


main :: IO ()
main = do
    filePath1 <- argOr 2023 "08-example.txt"
    contents1 <- readFile filePath1

    case Lib.main False contents1 of
        Left err     -> hPutStrLn stderr $ "could not parse input: " ++ show err
        Right result -> printf "ZZZ is reached in %d steps\n\n" result

    filePath2 <- argOr 2023 "08-example2.txt"
    contents2 <- readFile filePath2

    case Lib.main True contents2 of
        Left err     -> hPutStrLn stderr $ "could not parse input: " ++ show err
        Right result -> printf "all ghosts reach a *Z node in %d steps\n\n" result
