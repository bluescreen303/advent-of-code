module Main where

import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

import Helpers (argOr)
import qualified Year2023.Day07 as Lib


main :: IO ()
main = do
    filePath <- argOr 2023 "07-example.txt"
    contents <- readFile filePath

    case Lib.main False contents of
        Left err     -> hPutStrLn stderr $ "could not parse input: " ++ show err
        Right result -> printf "total winnings: %d\n\n" result

    case Lib.main True contents of
        Left err     -> hPutStrLn stderr $ "could not parse input: " ++ show err
        Right result -> printf "total winnings (alternate rules): %d\n\n" result
