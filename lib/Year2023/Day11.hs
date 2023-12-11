module Year2023.Day11 (main) where

import qualified Data.Set as Set
import Data.Bifunctor (first, second)

main :: Bool -> String -> Int
main x = sum
       . map (uncurry distance)
       . toPairs
       . expand (if x then 1_000_000 else 2)
       . toCoords
       . map (map (== '#'))
       . lines

expand :: Int -> [(Int, Int)] -> [(Int, Int)]
expand scale coords = foldr (\x fn -> map (second (\x' -> if x' > x then x' + scale - 1 else x')) . fn) id missingX
                    . foldr (\y fn -> map (first  (\y' -> if y' > y then y' + scale - 1 else y')) . fn) id missingY
                    $ coords
  where ys       = Set.fromList $ map fst coords
        xs       = Set.fromList $ map snd coords
        missingY = Set.fromAscList [0..maximum ys] `Set.difference` ys
        missingX = Set.fromAscList [0..maximum xs] `Set.difference` xs


distance :: (Int, Int) -> (Int, Int) -> Int
distance (y1, x1) (y2, x2) = abs (y2 - y1) + abs (x2 - x1)

toCoords :: [[Bool]] -> [(Int, Int)]
toCoords = concatMap (\(y, xs) -> map fst . filter snd . zipWith (\x -> ((y,x),)) [0..] $ xs)
         . zip [0..]

toPairs :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
toPairs []     = []
toPairs (x:xs) = map (x,) xs ++ toPairs xs
