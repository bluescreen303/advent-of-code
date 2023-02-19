module Day_2022_14 where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Text.Parsec hiding (Column, parse)
import qualified Text.Parsec as P
import Helpers (positiveNatural)
import Data.List (mapAccumL, find)
import Data.Semigroup (Max(..))

-- puzzle input, make invalid state unrepresentable

newtype Vertex = Vertex (Int, Int) deriving (Eq, Show)
data Path = Path Vertex Vertex [Vertex] deriving (Eq, Show)

-- input parsing

type Parser = Parsec String ()

parsePath :: Parser Path
parsePath = Path <$> parseVertex <* string " -> " <*> parseVertex
                 <*> many (string " -> " *> parseVertex)
  where parseVertex = curry Vertex <$> positiveNatural <* char ',' <*> positiveNatural

parse :: String -> Either ParseError [Path]
parse = P.parse (parsePath `sepEndBy1` endOfLine) ""

-- modeling the world

data VertBlock = VertBlock
    { startY :: Int
    , height :: Int
    }
    deriving (Show, Eq)

-- TODO: Would Seq be cheaper than List when inserting in the middle?
-- invariant: Column is a sorted list of non-overlapping VertBlocks
newtype Column = Column [VertBlock] deriving (Show, Eq)
newtype World  = World  (IntMap Column) deriving (Show, Eq)

pathSegments :: Path -> [(Vertex, Vertex)]
pathSegments (Path v1 v2 vs) = (v1, v2) : snd (mapAccumL go v2 vs)
    where go prv self = (self, (prv, self))

fromPaths :: [Path] -> World
fromPaths s = World $ foldr (uncurry go) IntMap.empty (concatMap pathSegments s)
    where go :: Vertex -> Vertex -> IntMap Column -> IntMap Column
          go (Vertex (fromX, fromY)) (Vertex (toX, toY)) w
              | fromY == toY = foldr (`insertBlock` VertBlock toY 0) w [(min fromX toX)..(max fromX toX)]
              | fromX == toX = insertBlock toX (VertBlock (min fromY toY) (abs $ fromY - toY)) w
              | otherwise    = error "path was neither horizontal nor vertical"

insertBlock :: Int -> VertBlock -> IntMap Column -> IntMap Column
insertBlock x v = IntMap.insertWith (<>) x (Column [v])

instance Semigroup Column where
    Column l <> Column r = Column (go l r)
        where go [] ys = ys
              go xs [] = xs
              go xxs@(x:xs) yys@(y:ys)
                  | startY x <= startY y
                    && startY x + height x >= startY y - 1 = go (combineL x y : xs) ys
                  | startY x <= startY y                   = x : go xs yys
                  | startY y + height y >= startY x - 1    = go xs (combineL y x : ys)
                  | otherwise                              = y : go xxs ys

              -- expects left input Y to be <= right input Y
              combineL (VertBlock xy xh) (VertBlock yy yh) = VertBlock xy (max (xy + xh) (yy + yh) - xy)

bottomOfWorld :: World -> Maybe Int
bottomOfWorld (World m) = getMax <$> foldMap col m
    where col :: Column -> Maybe (Max Int)
          col (Column []) = Nothing
          col (Column xs) = Just $ case last xs of
                              VertBlock y h -> Max $ y + h

-- mechanics

data FallResult = Stuck
                | Landed Vertex World
                | OffBottom
                deriving (Eq, Show)

landed :: FallResult -> Maybe Vertex
landed Stuck        = Nothing
landed OffBottom    = Nothing
landed (Landed v _) = Just v

fallOneColumn :: World -> Vertex -> FallResult
fallOneColumn (World w) (Vertex (x, y))  = maybe OffBottom fallInColumn (IntMap.lookup x w)
    where fallInColumn (Column blocks)   = maybe OffBottom foundInColumn
                                         $ find (\(VertBlock yy hh) -> yy + hh >= y) blocks
          foundInColumn (VertBlock yy _)
              | yy <= y                  = Stuck
              | otherwise                = Landed (Vertex (x, yy - 1))
                                                  (World $ IntMap.insertWith (<>) x (Column [VertBlock (yy - 1) 0]) w)

instance Semigroup FallResult where
    Stuck <> r = r
    l     <> _ = l

fall :: World -> Vertex -> FallResult
fall w v = let thisColumn = fallOneColumn w v in case thisColumn of
    Stuck                    -> Stuck
    OffBottom                -> OffBottom
    Landed (Vertex (x, y)) _ -> fall w (Vertex (x-1, y+1))
                             <> fall w (Vertex (x+1, y+1))
                             <> thisColumn -- laziness prevents calculating this column's world when falling sideways

simulation :: World -> [FallResult]
simulation w = let r = fall w (Vertex (500, 0))
               in r : case r of
                   Stuck       -> []
                   OffBottom   -> []
                   Landed _ w' -> simulation w'

main :: String -> Either ParseError Int
main = fmap (pred . length . simulation . fromPaths) . parse
