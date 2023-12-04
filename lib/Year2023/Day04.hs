module Year2023.Day04 where

import Data.List (mapAccumL)

import Text.Parsec
import Helpers

import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)

main :: Bool -> String -> Either ParseError Natural
main False = fmap points . doParse parser
main True  = fmap cards  . doParse parser

parser :: Parser [(Set Natural, Set Natural)]
parser = card `sepEndBy` endOfLine <* eof
  where toCard x y = (Set.fromList x, Set.fromList y)
        card       = toCard <$ sym "Card" <* positiveNatural
                            <* sym ":" <*> many1 (optSpaces positiveNatural')
                            <* sym "|" <*> many1 (optSpaces positiveNatural')


matches :: (Set Natural, Set Natural) -> Int
matches = Set.size . uncurry Set.intersection

points :: [(Set Natural, Set Natural)] -> Natural
points = sum . map (score . matches)
  where score 0 = 0
        score n = 2 ^ (n-1)

cards :: [(Set Natural, Set Natural)] -> Natural
cards = sum . snd . mapAccumL go (repeat 1)
  where go (num:nums) card = let (these, rest) = splitAt (matches card) nums
                             in (map (+ num) these ++ rest, num)
        go []         _    = ([], 0)
