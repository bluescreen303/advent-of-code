module Year2023.Day06 (main) where

import Text.Parsec
import Helpers

import Data.Char (digitToInt)
import Data.List (foldl')

main :: Bool -> String -> Either ParseError Int
main False = fmap (product . map ((\(x, y) -> y + 1 - x) . uncurry solve)) . doParse parser1
main True  = fmap                ((\(x, y) -> y + 1 - x) . uncurry solve)  . doParse parser2

solve :: Int -> Int -> (Int, Int)
solve time record = ( floor   ((-d + t) / 2) + 1
                    , ceiling (( d + t) / 2) - 1 )
  where d, t, r :: Double
        d = sqrt $ t ^ (2 :: Int) - 4 * r
        t = fromIntegral time
        r = fromIntegral record

parser1 :: Parser [(Int, Int)]
parser1 = zip <$> times    <* endOfLine
              <*> distance <* optional endOfLine <* eof
  where times    = sym "Time:"     *> many1 (optSpaces positiveNatural)
        distance = sym "Distance:" *> many1 (optSpaces positiveNatural)

parser2 :: Parser (Int, Int)
parser2 = (,) <$> time     <* endOfLine
              <*> distance <* optional endOfLine <* eof
  where time     = sym "Time:"     *> spacedNatural
        distance = sym "Distance:" *> spacedNatural
        spacedNatural = foldl' go 0 <$> many1 (digit <|> char ' ')
        go a ' ' = a
        go a i   = a * 10 + (fromIntegral . digitToInt $ i)
