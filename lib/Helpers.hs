module Helpers where

import Paths_advent_of_code (getDataFileName)
import System.Environment (getArgs)
import Text.Parsec
import Data.List (foldl')
import Data.Char (digitToInt)

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

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn n xs = case break (== n) xs of
                      (items, _:rest) -> items : splitOn n rest
                      (items, _)      -> [items]

splitPer :: Int -> [a] -> [[a]]
splitPer n xs = let (this, that) = splitAt n xs
                in this : case that of
                            [] -> []
                            _  -> splitPer n that

positiveNatural :: Stream s m Char => ParsecT s u m Int
positiveNatural = foldl' (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit
