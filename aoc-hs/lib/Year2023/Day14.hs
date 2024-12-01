{-# LANGUAGE DeriveAnyClass #-}
module Year2023.Day14 (main) where

import Data.List (transpose)
import Data.Hashable (Hashable(..))
import GHC.Generics (Generic)
import Helpers

main :: Bool -> String -> Int
main secondPuzzle = countRoundRocks
                  . takeWithLoopDetection (if secondPuzzle then 1_000_000_000 else 1)
                  . iterate               (if secondPuzzle then platformCycle else justNorth)
                  . map (map toRock)
                  . lines

data Rock = Round | Cube deriving (Eq, Generic, Hashable)

toRock :: Char -> Maybe Rock
toRock 'O' = Just Round
toRock '#' = Just Cube
toRock _   = Nothing

-- fromRock :: Maybe Rock -> Char
-- fromRock Nothing = '.'
-- fromRock (Just Round) = 'O'
-- fromRock (Just Cube)  = '#'

roll :: [Maybe Rock] -> [Maybe Rock]
roll xs = case break (== Just Cube) xs of
      ([], [])     -> []
      ([], r:est)  -> r : roll est
      (area, rest) -> process area ++ roll rest
  where process onlyRounds = take numAll (replicate numRounds (Just Round) ++ repeat Nothing)
          where (numAll, numRounds) = foldr go (0,0) onlyRounds
                go (Just Round) (!a, !r) = (a + 1, r + 1)
                go _            (!a, !r) = (a + 1, r)

justNorth :: [[Maybe Rock]] -> [[Maybe Rock]]
justNorth = transpose -- left becomes up
          . map roll
          . transpose -- up becomes left

platformCycle :: [[Maybe Rock]] -> [[Maybe Rock]]
platformCycle = map reverse
              . reverse
              . map roll  -- east
              . transpose
              . reverse
              . map roll  -- south
              . transpose
              . reverse
              . map roll  -- west
              . transpose
              . reverse
              . map roll  -- north
              . reverse
              . transpose

countRoundRocks :: [[Maybe Rock]] -> Int
countRoundRocks xs = sum
                   . zipWith (*) [length xs, (length xs - 1)..1]
                   . map (length . filter (== Just Round))
                   $ xs
