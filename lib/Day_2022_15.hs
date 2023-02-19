{-# LANGUAGE OverloadedLists #-}
module Day_2022_15 where

import Text.Parsec hiding (parse)
import qualified Text.Parsec as P
import Helpers (parseInt, mergeAll, without)

import Data.List (singleton, sort, nub)

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

--

main :: Maybe Int -> String -> Either ParseError Int
main mline = fmap (length . noBeacons mline) . parse

