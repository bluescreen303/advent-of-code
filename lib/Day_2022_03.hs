module Day_2022_03 where

import Data.List (intersect, nub)
import Data.Char (ord)

priority :: Char -> Int
priority c | 'a' <= c && c <= 'z' = ord c - ord 'a' + 1
priority c | 'A' <= c && c <= 'Z' = ord c - ord 'A' + 27
priority _ = error "invalid input"

splitList :: [a] -> ([a], [a])
splitList xs = splitAt (length xs `div` 2) xs

rucksackPriority :: String -> Int
rucksackPriority = sum
                 . map priority
                 . nub
                 . uncurry intersect
                 . splitList

main :: String -> Int
main = sum
     . map rucksackPriority
     . lines

-- intersect               :: (Eq a) => [a] -> [a] -> [a]
-- intersect               =  intersectBy (==)

-- intersectBy             :: (a -> a -> Bool) -> [a] -> [a] -> [a]
-- intersectBy _  [] _     =  []
-- intersectBy _  _  []    =  []
-- intersectBy eq xs ys    =  [x | x <- xs, any (eq x) ys]