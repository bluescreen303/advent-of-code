module Day_2022_04 where

import Data.Char (digitToInt)
import Data.List (foldl', (\\))
import Text.Parsec

parser :: Stream s m Char => ParsecT s u m [([Int], [Int])]
parser = line `sepEndBy` endOfLine <* eof
    where line = (,) <$> range positiveNatural <* char ',' <*> range positiveNatural
          range p = enumFromTo <$> p <* char '-' <*> p
          positiveNatural = foldl' (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit

fullyContains :: Eq a => [a] -> [a] -> Bool
fullyContains xs ys = null (ys \\ xs)
                   || null (xs \\ ys)

doParse :: String -> Either ParseError [([Int], [Int])]
doParse = parse parser ""

main :: String -> Either ParseError Int
main = fmap go . doParse
    where go = length
             . filter (uncurry fullyContains)

main2 :: String -> [([Int], [Int])]
main2 = either (error . show) id . doParse