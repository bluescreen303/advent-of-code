module Year2023.Day13 (main) where

import Helpers

import Data.List (find, transpose)
import Control.Applicative ((<|>))
import Data.Maybe (mapMaybe)

data MirrorLine = Row Int | Col Int deriving Show

main :: Bool -> String -> Int
main secondPuzzle = sum
                  . map score
                  . mapMaybe (solve $ if secondPuzzle then 1 else 0)
                  . parse

score :: MirrorLine -> Int
score (Row x) = 100 * x
score (Col x) = x

solve :: Eq a => Int -> [[a]] -> Maybe MirrorLine
solve numSmudges input = go Row input <|> go Col (transpose input)
  where go constructor = fmap (constructor . fst)                     -- take line number and map to constructor
                       . find (uncurry (isMirrored numSmudges) . snd) -- check mirroring on all options
                       . zip [1..]                                    -- add line numbers (from 1 because we skip first)
                       . mirrorRows

isMirrored :: Eq a => Int -> [[a]] -> [[a]] -> Bool
isMirrored numSmudges left right = length found == numSmudges && null rest
  where (found, rest) = splitAt numSmudges
                      . filter not
                      $ zipWith (==) (concat $ take n left) (concat $ take n right)
        n             = length left `min` length right

mirrorRows :: [a] -> [([a],[a])]
mirrorRows = tail -- drop first (mirror line at border)
           . init -- drop last  (mirror line at border)
           . go []
  where go before []       = [(before, [])]
        go before (a:fter) = (before, a:fter) : go (a:before) fter

parse :: String -> [[[Bool]]]
parse = splitOn [] . map (map (=='#')) . lines
