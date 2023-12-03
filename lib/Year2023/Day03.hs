module Year2023.Day03 where

import Data.List (groupBy, mapAccumL)
import Data.Char (isDigit)
import Data.Bifunctor (first, second)
import Data.Maybe (mapMaybe)

-- helper types to not mix up dimensions
newtype X = X Int deriving (Num, Eq, Ord, Enum)
newtype Y = Y Int deriving (Num, Eq, Ord, Enum)
data Coord = Coord Int Int deriving Eq

toCoord :: Y -> X -> Coord
toCoord (Y y) (X x) = Coord y x


main :: Bool -> String -> Int
main secondPuzzle = let handler = if secondPuzzle then getGear else getPart
                    in sum
                     . mapMaybe handler
                     . withNeighbours
                     . parse

getPart :: (Coord, (String, [(Coord, String)])) -> Maybe Int
getPart (_, (str, nbs)) | isDigit (head str) && not (null nbs) = Just (read str)
                        | otherwise                            = Nothing

getGear :: (Coord, (String, [(Coord, String)])) -> Maybe Int
getGear (_, ("*", nbs)) = case filter (isDigit . head) (map snd nbs) of
                            [part1, part2] -> Just (read part1 * read part2)
                            _              -> Nothing
getGear _               = Nothing

withNeighbours :: [(Y, [(X, String)])] -> [(Coord, (String, [(Coord, String)]))]
withNeighbours input = flattenCoords . snd . mapAccumL goLine ((0,[]):input) $ input
  where goLine neighbourLines (y, xs) = (tail neighbourLines, (y, map goItem xs))

          where goItem (x, str) = (x, (str, withoutSelf
                                          . flattenCoords
                                          . map (second neighbourCols)
                                          . take 3
                                          $ neighbourLines ))

                  where withoutSelf   = filter ((/= toCoord y x) . fst)
                        neighbourCols = takeWhile ((<= x + X (length str)) . fst)
                                      . dropWhile (\(x', str') -> x' + X (length str') < x)

flattenCoords :: [(Y, [(X, a)])] -> [(Coord, a)]
flattenCoords = concatMap (\(y, xs) -> map (first (toCoord y)) xs)

parse :: String -> [(Y, [(X, String)])]
parse = zip [0..] . (map processLine . lines)

processLine :: String -> [(X, String)]
processLine = filter (not . isDot)
            . snd . mapAccumL go 0 -- count x position in line
            . groupBy groupChars
  where go :: X -> String -> (X, (X, String))
        go x value  = (x + X (length value), (x, value))

        isDot (_, '.':_) = True
        isDot _          = False

groupChars :: Char -> Char -> Bool
groupChars '.' '.'         = True
groupChars x   y
  | isDigit x && isDigit y = True
  | otherwise              = False
