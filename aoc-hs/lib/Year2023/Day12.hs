module Year2023.Day12 where

import Data.Bifunctor (bimap)
import Data.List (intercalate, tails)

import Text.Parsec

import Helpers

main :: Bool -> String -> Either ParseError Int
main secondPuzzle = fmap ( sum
                         . map ( uncurry (flip solve)
                               . bimap unfoldNumbers unfoldSpringRecs
                               )
                         ) . doParse parser

  where unfoldFactor     = if secondPuzzle then 5 else 1
        unfoldNumbers    = concat . replicate unfoldFactor
        unfoldSpringRecs = intercalate [Unknown] . replicate unfoldFactor

data SpringRecord = Operational | Damaged | Unknown deriving (Eq, Show)

parser :: Parser [([Int], [SpringRecord])]
parser = line `sepEndBy1` endOfLine <* eof
  where line      = flip (,) <$> many1 springRec <* char ' ' <*> numbers
        springRec = Operational <$ char '.'
                <|> Damaged     <$ char '#'
                <|> Unknown     <$ char '?'
        numbers   = positiveNatural `sepBy1` char ','

solve :: [SpringRecord] -> [Int] -> Int
solve recs = head . head . table recs

-- +-------------+---+---+---+---+---+---+---+---+---+
-- |             | ? | ? | ? | . | # | # | # | . | _ |
-- +=============+===+===+===+===+===+===+===+===+===+
-- | [ ]         | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 1 | 1 |
-- +-------------+---+---+---+---+---+---+---+---+---+
-- | [ 3 ]       | 1 | 1 | 1 | 1 | 1 | 0 | 0 | 0 | 0 |
-- +-------------+---+---+---+---+---+---+---+---+---+
-- | [ 1; 3 ]    | 3 | 2 | 1 | 0 | 0 | 0 | 0 | 0 | 0 |
-- +-------------+---+---+---+---+---+---+---+---+---+
-- | [ 1; 1; 3 ] | 1 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
-- +-------------+---+---+---+---+---+---+---+---+---+
-- table[y][x] =
--     n = numbers[0]
--     match string[x] with
--         | '.' -> table[y][x + 1]
--         | '#' -> if can_match string[x..+n] n
--                  then table[y - 1][x + n + 1]
--                  else 0
--         | '?' -> case '.' + case '#'

table :: [SpringRecord] -> [Int] -> [[Int]]
table recs = foldr goLine [] . tails
  where goLine :: [Int] -> [[Int]] -> [[Int]]
        goLine []    rest = init (scanr go0    1 (                          tails recs)) : rest
        goLine (n:_) rest = init (scanr (go n) 0 (zip (tails $ head rest) $ tails recs)) : rest

        go0 :: [SpringRecord] -> Int -> Int
        go0 (Damaged:_) _    = 0
        go0 _           next = next

        go :: Int -> ([Int], [SpringRecord]) -> Int -> Int
        go _ (_, [])                _    = 0
        go _ (_, Operational:_)     next = next
        go n (above, s@(Damaged:_)) _
          | canMatch n s                 = last $ take (n+2) above
          | otherwise                    = 0
        go n (above, Unknown:rest)  next = go n (above, Operational:rest) next
                                         + go n (above, Damaged:rest) next

        canMatch n s = case splitAt n s of
                         (_, Damaged:_) -> False
                         (this, _)      -> length this == n && Operational `notElem` this
