module Main where

import Text.Printf (printf)

import Helpers (argOr)
import qualified Day_2022_05 as Lib

main :: IO ()
main = do
    filePath <- argOr "2022-05-example.txt"
    contents <- readFile filePath

    case Lib.main contents of
        Left  err      -> error $ "parse error " ++ show err
        Right Nothing  -> error "logic error while moving crates"
        Right (Just r) -> printf "top of stack is now: %s\n\n" r
