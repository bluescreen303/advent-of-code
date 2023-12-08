module Year2023.Day08 (main) where

import Text.Parsec
import Helpers

import Data.Maybe (fromJust)
import Data.List (scanl')

main :: Bool -> String -> Either ParseError Int
main secondPuzzle = fmap ( foldr lcm 1
                         . uncurry findLoopLength
                         . uncurry toMaze )
                  . doParse parser
  where findLoopLength dirs = map (length . takeWhile (not . isEndNode . here) . (`walk` dirs))
                            . filter (isStartNode . here)
        isStartNode | secondPuzzle = (== 'A') . last
                    | otherwise    = (== "AAA")
        isEndNode   | secondPuzzle = (== 'Z') . last
                    | otherwise    = (== "ZZZ")

data Dir = L | R deriving Show

parser :: Parser ([Dir], [(String, (String, String))])
parser = (,) <$> many1 dir <* endOfLine <* endOfLine
             <*> edges `sepEndBy1` endOfLine
             <*  eof
  where dir   = L <$ char 'L' <|> R <$ char 'R'
        edges = (,) <$> node
                    <*  sym "="
                    <*> between (sym "(") (sym ")")
                                ((,) <$> node <* sym "," <*> node)
        node  = optSpaces (many1 alphaNum)

data Maze a = Node { here :: !a, goLeft :: Maze a, goRight :: Maze a}

step :: Dir -> Maze a -> Maze a
step L = goLeft
step R = goRight

walk :: Maze a -> [Dir] -> [Maze a]
walk = scanl' (flip step)

toMaze :: [Dir] -> [(String, (String, String))] -> ([Dir], [Maze String])
toMaze dirs edges = (cycle dirs, map snd nodes)
  where nodes  = map (\(self, (left, right)) -> (self, Node self (node left) (node right))) edges
        node n = fromJust $ lookup n nodes
