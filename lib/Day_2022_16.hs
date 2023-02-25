{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Day_2022_16 (Valve(..), main, parse) where
import qualified Text.Parsec as P
import Text.Parsec hiding (parse)
import Helpers (positiveNatural, symbol, lexeme, commaSep, enum)
import Data.Maybe (fromJust, fromMaybe)
import Data.Foldable (find)
import Grid (mkSomeGrid, Grid, grid1D, addDimension, findIndex)
import GHC.TypeLits (KnownNat)
import Sized (Indexed (set, get), Index, imap)
import TypeLevel (HList(..), Finite, hHead)
import Data.List (delete)


-- input parsing

type Parser = Parsec String ()
data Valve = Valve { valveName :: String
                   , valveRate :: Int
                   , valveConnections :: [String] }
                   deriving (Eq, Show)

parseValve :: Parser Valve
parseValve = Valve <$> (symbol "Valve" *> lexeme (many1 upper))
                   <*> (symbol "has flow rate=" *> positiveNatural <* symbol ";")
                   <*> (symbol' "tunnel" *> symbol' "lead" *> symbol "to" *> symbol' "valve" *> commaSep (many1 upper))
    where symbol' x = lexeme (string x <* optional (char 's'))

parse :: String -> Either ParseError [Valve]
parse = P.parse (parseValve `sepEndBy1` endOfLine) ""

data Puzzle x = Puzzle { rates       :: Grid '[x] Int
                       , connections :: Grid '[x, x] (Maybe Int)
                       , startPos    :: Finite x
                       }

floydWarshall :: forall x t. (KnownNat x, Num t, Ord t) => Grid [x, x] (Maybe t) -> Grid [x, x] (Maybe t)
floydWarshall initial = foldr go initial ((,,) <$> enum <*> enum <*> enum)
    where go :: (Finite x, Finite x, Finite x) -> Grid '[x, x] (Maybe t) -> Grid '[x, x] (Maybe t)
          go (k, i, j) m = case (get (ii i k) m, get (ii k j) m, get (ii i j) m) of
                             (Just a, Just b, Nothing)      -> set (Just $ a + b) (ii i j) m
                             (Just a, Just b, Just current) -> set (Just $ min current (a + b)) (ii i j) m
                             _                              -> m
          ii :: Finite x -> Finite y -> Index [x, y]
          ii a b = a :| b :| Nil

search :: forall x. KnownNat x => Bool -> Int -> Puzzle x -> Int
search withElephant maxTime (Puzzle rates connections startPos) = go withElephant maxTime startPos initialTodo
    where initialTodo = filter (\i -> get (i :| Nil) rates > 0) enum
          go :: Bool -> Int -> Finite x -> [Finite x] -> Int
          go el timeLeft currentLoc todo
              = maximum
              . (:) (if el then go False 26 startPos todo else 0)
              . map (\v -> let t = timeLeft - fromJust (get (currentLoc :| v :| Nil) connections) - 1
                           in get (v :| Nil) rates * t + go el t v (delete v todo))
              . filter (\v -> maybe False (< timeLeft) (get (currentLoc :| v :| Nil) connections))
              $ todo

typedMain :: KnownNat x => Grid '[x] Valve -> (Int, Int)
typedMain valves = (search False 30 puzzle, search True 26 puzzle)
    where go (Valve _ _ cs) = imap (\i _ -> 1 <$ find (\c -> findIndex ((==c) . valveName) valves == Just i) cs) valves
          puzzle            = Puzzle { rates       = fmap valveRate valves
                                     , connections = floydWarshall . addDimension . fmap go $ valves
                                     , startPos    = hHead . fromMaybe (error "no valve named 'AA'")
                                                   . findIndex (\v -> valveName v == "AA") $ valves }

main :: String -> Either ParseError (Int, Int)
main = fmap (grid1D typedMain . valveGrid) . parse
    where valveGrid = fromMaybe (error "impossible: one-dimensional list should always be valid!")
                    . mkSomeGrid @1

-- floydWarshall' :: (Ord a, Num b, Ord b) => [a] -> Map (a, a) b -> Map (a, a) b
-- floydWarshall' xs initial = foldr go initial ((,,) <$> xs <*> xs <*> xs)
--     where go (k, i, j) m = maybe m
--                                  (\b -> Map.insertWith min (i, j) b m)
--                                  ((+) <$> (m !? (i, k)) <*> (m !? (k, j)))

