module Day_2022_01 where

import Data.List (maximumBy)
import Data.Function (on)
import Control.Arrow (second)

import Helpers (grouped)

readGroups :: Read r => [(a, [String])] -> [(a, [r])]
-- readGroups = map $ \(n, items) -> (n, map read items)
readGroups = withGroups $ map read

withGroups :: (b -> c) -> [(d, b)] -> [(d, c)]
withGroups = map . second

main :: String -> (Integer, Integer)
main = maximumBy (compare `on` snd)
     . withGroups (sum . map read)
     . grouped
