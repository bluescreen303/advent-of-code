module Year2022.Day03 where

import Data.List (intersect, nub)
import Data.Char (ord)

priority :: Char -> Int
priority c | 'a' <= c && c <= 'z' = ord c - ord 'a' + 1
priority c | 'A' <= c && c <= 'Z' = ord c - ord 'A' + 27
priority _ = error "invalid input"

splitList :: [a] -> ([a], [a])
splitList xs = splitAt (length xs `div` 2) xs

itemsPriority :: String -> Int
itemsPriority = sum
              . map priority
              . nub

rucksackPriority :: String -> Int
rucksackPriority = itemsPriority
                 . uncurry intersect
                 . splitList

main :: String -> Int
main = sum
     . map rucksackPriority
     . lines

main2 :: String -> Int
main2 = sum
      . map (itemsPriority . foldr1 intersect)
      . chunksOf 3
      . lines

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
    let (ys, zs) = splitAt n xs
    in  ys : chunksOf n zs

-- intersect               :: (Eq a) => [a] -> [a] -> [a]
-- intersect               =  intersectBy (==)

-- intersectBy             :: (a -> a -> Bool) -> [a] -> [a] -> [a]
-- intersectBy _  [] _     =  []
-- intersectBy _  _  []    =  []
-- intersectBy eq xs ys    =  [x | x <- xs, any (eq x) ys]