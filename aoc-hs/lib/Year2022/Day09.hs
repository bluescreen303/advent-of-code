module Year2022.Day09 where

import Data.List (scanl', nub)

-- parse the puzzle
type Move = (Int, Int) -> (Int, Int)

toMove :: [String] -> [Move]
toMove [d, c] = replicate a (m d)
    where m "U" (x, y) = (x, y + 1)
          m "D" (x, y) = (x, y - 1)
          m "L" (x, y) = (x - 1, y)
          m "R" (x, y) = (x + 1, y)
          m e   _      = error $ "unknown move: " ++ e
          a = read c
toMove e      = error $ "unknown move: " ++ unwords e

parse :: String -> [Move]
parse = concatMap (toMove . words) . lines

-- run the rope

headLocations :: [Move] -> [(Int, Int)]
headLocations = scanl' (flip id) (0, 0)

nextKnotLocations :: [(Int, Int)] -> [(Int, Int)]
nextKnotLocations = tail . scanl' moveTail (0, 0)
    where moveTail t@(tailX, tailY) (headX, headY)
              | distance <= 1 = t
              | otherwise     = (tailX + signum diffX, tailY + signum diffY)
            where diffX    = headX - tailX
                  diffY    = headY - tailY
                  distance = max (abs diffX) (abs diffY)

main :: String -> Int
main = length . nub . foldr1 (.) (replicate 9 nextKnotLocations) . headLocations . parse
