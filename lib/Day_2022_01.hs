module Day_2022_01 where

import Data.List (sortBy)
import Data.Function (on)
import Control.Arrow (second)

import Helpers (grouped)

readGroups :: Read r => [(a, [String])] -> [(a, [r])]
-- readGroups = map $ \(n, items) -> (n, map read items)
readGroups = withGroups $ map read

withGroups :: (b -> c) -> [(d, b)] -> [(d, c)]
withGroups = map . second

sortedElves :: String -> [(Integer, Integer)]
sortedElves = sortBy (flip compare `on` snd)
            . withGroups (sum . map read)
            . grouped

main :: String -> (Integer, Integer)
main = head
     . sortedElves

main2 :: String -> Integer
main2 = sum
      . map snd
      . take 3
      . sortedElves
