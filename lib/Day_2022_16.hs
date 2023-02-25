module Day_2022_16 (main, parse) where
import qualified Text.Parsec as P
import Text.Parsec hiding (parse)
import Helpers (positiveNatural, symbol, lexeme, commaSep)

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!), (!?))

-- input parsing

type Parser = Parsec String ()
data Valve = Valve { valveName :: String
                   , valveRate :: Int
                   , valveConnections :: [String] }

parseValve :: Parser Valve
parseValve = Valve <$> (symbol "Valve" *> lexeme (many1 upper))
                   <*> (symbol "has flow rate=" *> positiveNatural <* symbol ";")
                   <*> (symbol' "tunnel" *> symbol' "lead" *> symbol "to" *> symbol' "valve" *> commaSep (many1 upper))
    where symbol' x = lexeme (string x <* optional (char 's'))

parse :: String -> Either ParseError [Valve]
parse = P.parse (parseValve `sepEndBy1` endOfLine) ""

--

-- TODO: by turning our input strings into Int, we can turn Map into Vec
-- TODO: we could halve the work by only looking for (i,j) | i < j. This can even map to a diagonal 2D vec :)
floydWarshall :: (Ord a, Num b, Ord b) => [a] -> Map (a, a) b -> Map (a, a) b
floydWarshall xs initial = foldr go initial loop
    where go (k, i, j) m = maybe m (\b -> Map.insertWith min (i, j) b m) ((+) <$> (m !? (i, k)) <*> (m !? (k, j)))
          loop = [(k, i, j) | k <- xs, i <- xs, i /= k, j <- xs, j /= k, j /= i]
                    

prep :: [Valve] -> (Map String Int, Map (String, String) Int)
prep valves = let (rates, connections) = foldr go (Map.empty, Map.empty) valves
              in  (rates, floydWarshall (map valveName valves) connections)
    where go (Valve name rate connections) (rates, c) =
              (Map.insert name rate rates, foldr (\conn -> Map.insert (conn, name) 1) c connections)

main :: String -> Either ParseError (Map String Int, Map (String, String) Int)
main = fmap prep . parse

