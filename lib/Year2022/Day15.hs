module Year2022.Day15 (Sensor(..), Vertex(..), parse, main, main1, hidingSpots) where

import Text.Parsec hiding (Line, parse)
import qualified Text.Parsec as P
import Helpers (parseInt, mergeAll, without)

import Data.List (singleton, sort, nub, group, partition)

newtype Vertex = Vertex (Int, Int) deriving (Eq, Show)

instance Ord Vertex where
  compare (Vertex (x1, y1)) (Vertex (x2, y2)) = compare (y1, x1) (y2, x2)

data Sensor = Sensor { location :: Vertex, nearestBeacon :: Vertex } deriving (Eq, Show)

-- input parsing

type Parser = Parsec String ()

parseSensor :: Parser Sensor
parseSensor = Sensor <$ string "Sensor at "              <*> parseVertex
                     <* string ": closest beacon is at " <*> parseVertex
  where parseVertex = curry Vertex <$ string "x=" <*> parseInt
                                   <* string ", "
                                   <* string "y=" <*> parseInt

parse :: String -> Either ParseError [Sensor]
parse = P.parse (parseSensor `sepEndBy1` endOfLine) ""

-- modeling the world

distance :: Vertex -> Vertex -> Int
distance (Vertex (x1, y1)) (Vertex (x2, y2)) = abs (x1 - x2) + abs (y1 - y2)

scanRange :: Sensor -> Int
scanRange (Sensor s b) = distance s b

covers :: Vertex -> Sensor -> Bool
covers v s@(Sensor pos _) = distance pos v <= scanRange s

-- working with areas to enumerate over
-- this turns out to be too slow and loop-intensive (albeit in constant memory)

area :: Maybe Int -> Sensor -> [Vertex]
area mline me@(Sensor (Vertex (x, y)) _) = do
        yy <- maybe [y-d..y+d] singleton mline
        let rest = d - abs (y - yy) `max` 0
        xx <- [x-rest..x+rest]
        return $ Vertex (xx, yy)
    where d = scanRange me

allAreas :: Maybe Int -> [Sensor] -> [Vertex]
allAreas mline = mergeAll . map (area mline)

noBeacons :: Maybe Int -> [Sensor] -> [Vertex]
noBeacons mline sensors = allAreas mline sensors `without` beacons
    where beacons = nub . sort . map nearestBeacon $ sensors

-- change strategy to work with lines and intersections
-- we can calculate the four lines that pass a sensor-detection-area
-- in the NW,NE,SE,SW. We have to expand those lines by 1 to find
-- the lines that may contain uncovered areas. In theory, these
-- areas can be large / multi-vertex, which would still put us in
-- area-calculating mode. But since the puzzle mentions there is
-- only a single vertex, it's good enough to look for a vertex
-- at the "just 1 out" intersection of 4 sensor-ranges.

-- represents y = mx + q -- slope = m, displacement = q
data Line = Line { slope :: Int, displacement :: Int } deriving (Eq, Ord, Show)

bounds :: Int -> Sensor -> [Line]
bounds offset s@(Sensor (Vertex (x, y)) _) = [ Line 1    $ y - r - x
                                             , Line 1    $ y + r - x
                                             , Line (-1) $ y - r + x
                                             , Line (-1) $ y + r + x
                                             ]
    where r = scanRange s + offset

innerBounds, outerBounds :: Sensor -> [Line]
innerBounds = bounds 0
outerBounds = bounds 1

-- intersect should normally use:
-- y = a x + b  AND y = c x + d
-- a x + b = c x + d
-- a x = c x + d - b
-- a x - c x = d - b
-- x (a - c) = d - b
-- x = (d - b) / (a - c)
-- in this case, dealing with integers, it only really works for diagonal lines
-- which happens to be OK for this puzzle
intersect :: Line -> Line -> Vertex
intersect (Line a b) (Line c d)
    | a == c    = error "looking for intersection of parallel lines"
    | otherwise = let x = (d - b) `div` (a - c)
                      y = a * x + b
                  in Vertex (x, y)


hidingSpots :: Int -> [Sensor] -> [Vertex]
hidingSpots m sensors = filter (not . (`any` sensors) . covers) options
    where options     = filter inRange $ intersect <$> asc <*> desc
          inRange (Vertex (x, y)) = x >= 0 && y >= 0 && x <= m && y <= m
          (asc, desc) = partition (\x -> slope x > 0)
                      -- we only need lines that overlap for this puzzle
                      -- this is because one sensor's NW will be another sensor's SE
                      . map head
                      . filter (\x -> length x > 1)
                      . group
                      --
                      . sort
                      . concatMap outerBounds
                      $ sensors

--

main1 :: Maybe Int -> String -> Either ParseError Int
main1 mline = fmap (length . noBeacons mline) . parse


--

tuningFrequency :: Vertex -> Int
tuningFrequency (Vertex (x, y)) = 4000000 * x + y

hidingSpot :: Int -> [Sensor] -> Maybe Int
hidingSpot m sensors = case hidingSpots m sensors of
                         [v] -> Just $ tuningFrequency v
                         _   -> Nothing

--

main :: Int -> String -> Either ParseError (Maybe Int)
main m = fmap (hidingSpot m) . parse
