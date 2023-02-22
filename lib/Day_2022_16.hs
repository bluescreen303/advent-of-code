module Day_2022_16 (main, parse, ValveDef(..)) where
import qualified Text.Parsec as P
import Text.Parsec hiding (parse)
import Helpers (positiveNatural, symbol, lexeme, commaSep)

import Data.Set (Set, notMember, insert, isSubsetOf, (\\))
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import Data.Foldable (maximumBy)
import Data.Function (on)
import Control.Parallel.Strategies (withStrategy, parTraversable, rseq, Strategy, evalTraversable)

-- input model

data ValveDef = ValveDef { valveFlowRate :: Int, valveConnections :: [String]} deriving (Eq, Show)
type WorldMap = Map String ValveDef

-- input parsing

type Parser = Parsec String ()

parseValveDef :: Parser (String, ValveDef)
parseValveDef = (,) <$> (symbol "Valve" *> lexeme (many1 upper))
                    <*> (ValveDef <$ symbol "has flow rate=" <*> positiveNatural <* symbol ";"
                                  <* symbol' "tunnel" <* symbol' "lead" <* symbol "to" <* symbol' "valve"
                                  <*> commaSep (many1 upper))
    where symbol' x = lexeme (string x <* optional (char 's'))

parse :: String -> Either ParseError WorldMap
parse = fmap Map.fromList . P.parse (parseValveDef `sepEndBy1` endOfLine) ""

--

data WorldState = WorldState { myLocation :: String
                             , elephantLocation :: String
                             , openedValves :: Set String
                             , pressureReleased :: Int }
                             deriving (Eq, Show)

advance :: WorldMap -> WorldState -> WorldState
advance wm ws = ws { pressureReleased = pressureReleased ws + increase }
    where increase = sum $ Set.map (\v -> valveFlowRate (wm ! v)) (openedValves ws)

myValidMoves :: WorldMap -> WorldState -> Set WorldState
myValidMoves wm ws@(WorldState l _ ov _) =
    foldr (\l' s -> ws { myLocation = l' } `insert` s)
          (if l `notMember` ov && rate > 0 -- open a closed, non-zero valve
           then Set.singleton ws { openedValves = l `insert` ov }
           else Set.empty)
          connections -- move to any other connected location
    where ValveDef rate connections = wm ! l

elephantValidMoves :: WorldMap -> WorldState -> Set WorldState
elephantValidMoves wm ws@(WorldState _ e ov _) =
    foldr (\e' s -> ws { elephantLocation = e' } `insert` s)
          (if e `notMember` ov && rate > 0 -- open a closed, non-zero valve
           then Set.singleton ws { openedValves = e `insert` ov }
           else Set.empty)
          connections -- move to any other connected location
    where ValveDef rate connections = wm ! e

validMoves :: WorldMap -> WorldState -> Set WorldState
validMoves wm ws = Set.unions . Set.map (elephantValidMoves wm) $ myValidMoves wm ws

-- it's hard to come up with a proper pruning strategy. Depending on the worldmap, visiting the same location multiple
-- times may not be a bad idea (let's say there's a map with a central hub). Going back to where you came is also
-- a valid move in such a situation. Opening far less valves than others may not be bad either. Perhaps it's the only
-- way to get to a far-off valve which would instantly release all pressure.
-- So the chosen strategy is to prune states that are at the same location as others, with only a subset of valves
-- opened, with less pressure released.
prune :: Set WorldState -> Set WorldState
prune wss = wss \\ redundants
    where redundants = Set.unions
                     . map snd
                     . Map.toList
                     . withStrategy (parTraversable evalWorldStates)
                     . fmap pruneStatesAtSameLocation
                     . foldr (\ws m -> Map.insertWith Set.union (myLocation ws, elephantLocation ws) (Set.singleton ws) m) Map.empty
                     $ wss

          pruneStatesAtSameLocation :: Set WorldState -> Set WorldState
          pruneStatesAtSameLocation xs = Set.filter (\x -> any (x `madeRedundantBy`) $ Set.delete x xs) xs

          madeRedundantBy :: WorldState -> WorldState -> Bool
          madeRedundantBy (WorldState _ _ ov1 pr1) (WorldState _ _ ov2 pr2) = pr1 <= pr2
                                                                           && ov1 `isSubsetOf` ov2
          evalWorldStates :: Strategy (Set WorldState)
          evalWorldStates s = do l <- evalTraversable rseq (Set.toList s)
                                 s' <- rseq s
                                 return (if length l > (-1) then s else s')

step :: WorldMap -> Set WorldState -> Set WorldState
step wm = prune . Set.unions . Set.map (validMoves wm . advance wm)

play :: WorldMap -> [Set WorldState]
play wm = iterate (step wm) (Set.singleton startState)
    where startState = WorldState "AA" "AA" Set.empty 0

main :: String -> Either ParseError Int
main = fmap ( pressureReleased
            . maximumBy (compare `on` pressureReleased)
            . (!! 26)
            . play )
     . parse

instance Ord WorldState where
    compare (WorldState l1 e1 ov1 pr1) (WorldState l2 e2 ov2 pr2) = compare (l1, e1, pr2, ov2) (l2, e2, pr1, ov1)
