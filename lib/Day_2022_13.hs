module Day_2022_13 where

import Text.Parsec ( ParseError, Parsec, char, endOfLine, between, sepBy, (<|>) )
import qualified Text.Parsec as P
import Helpers (positiveNatural)
import Data.List (sort)

type Parser = Parsec String ()

data Tree a = Leaf a
            | Node [Tree a]
  deriving Show

instance Eq a => Eq (Tree a) where
    Leaf a     == Leaf b     = a == b
    Node as    == Node bs    = as == bs
    a@(Leaf _) == b@(Node _) = Node [a] == b
    a@(Node _) == b@(Leaf _) = a == Node [b]

instance Ord a => Ord (Tree a) where
    Leaf a     <= Leaf b     = a <= b
    Node as    <= Node bs    = as <= bs
    a@(Leaf _) <= b@(Node _) = Node [a] <= b
    a@(Node _) <= b@(Leaf _) = a <= Node [b]

parseTree :: Parser (Tree Int)
parseTree = parseLeaf <|> parseNode
    where parseLeaf = Leaf <$> positiveNatural
          parseNode = Node <$> between (char '[')
                                       (char ']')
                                       (parseTree `sepBy` char ',')

parseFile :: Parser [(Tree Int, Tree Int)]
parseFile = ((,) <$> parseTree <* endOfLine <*> parseTree <* endOfLine)
            `sepBy` endOfLine

parse :: String -> Either ParseError [(Tree Int, Tree Int)]
parse = P.parse parseFile ""

main :: String -> Either ParseError Int
main = fmap go . parse
    where go :: Ord a => [(a, a)] -> Int
          go = sum
             . map fst
             . filter (uncurry (<=) . snd)
             . zip [1..]

main2 :: String -> Either ParseError Int
main2 = fmap go . parse
    where go = product
             . map fst
             . filter ((`elem` [divider1, divider2]) . snd)
             . zip [1..]
             . sort
             . concatMap (\(a, b) -> [a, b])
             . ((divider1, divider2) :)

divider1 :: Tree Int
divider1 = Node [Node [Leaf 2]]

divider2 :: Tree Int
divider2 = Node [Node [Leaf 6]]