module Year2023.Day06 (main) where

import Text.Parsec
import Helpers

import Data.Char (digitToInt)
import Data.List (foldl')

main :: Bool -> String -> Either ParseError Int
main False = fmap (product . map (length . uncurry winningStrategies)) . doParse parser1
main True  = fmap                (length . uncurry winningStrategies)  . doParse parser2

winningStrategies :: Int -> Int -> [(Int, Int)]
winningStrategies timeLimit record  = filter ((> record) . snd) . map (\x -> (x, go x)) $ [0..timeLimit]
  where go buttonHeld = (timeLimit - buttonHeld) * buttonHeld

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
