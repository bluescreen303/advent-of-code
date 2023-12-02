module Year2023.Day02 where

import Text.Parsec
import Helpers

type RGB = [Int]

data Game = Game { gameId :: Int, showings :: [RGB] } deriving Show

parser :: Parser [Game]
parser = game `sepEndBy` endOfLine <* eof
  where game    = Game <$ sym "Game" <*> positiveNatural
                       <* sym ":"    <*> showing `sepBy` sym ";"
        showing = foldr1 (zipWith (+)) <$> colour `sepBy` sym ","
        colour  = flip ($) <$> optSpaces positiveNatural
                           <*> (  (\x -> [x, 0, 0]) <$ sym "red"
                              <|> (\x -> [0, x, 0]) <$ sym "green"
                              <|> (\x -> [0, 0, x]) <$ sym "blue"  )

main :: Bool -> String -> Either ParseError Int
main False = fmap (sum . puzzle1 [12, 13, 14]) . doParse parser
main True  = fmap (sum . puzzle2)              . doParse parser

puzzle1 :: RGB -> [Game] -> [Int]
puzzle1 x = map gameId . filter (all (and . zipWith (>=) x) . showings)

puzzle2 :: [Game] -> [Int]
puzzle2 = map (product . foldr1 (zipWith max) . showings)
