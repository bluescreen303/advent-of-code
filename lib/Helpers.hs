module Helpers where

import Paths_advent_of_code (getDataFileName)
import System.Environment (getArgs)

splitAtEmpty :: [String] -> [[String]]
splitAtEmpty l = case break (== "") l of
                     (items, "":rest) -> items : splitAtEmpty rest
                     (items, _)       -> [items]

grouped :: String -> [(Integer, [String])]
grouped = zip [0..] . splitAtEmpty . lines

argOr :: FilePath -> IO String
argOr dataFileName = do
    args <- getArgs
    case args of
        [f] -> return f
        _   -> getDataFileName dataFileName
