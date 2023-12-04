module Year2023.Day03 where

import Data.List (groupBy, mapAccumL)
import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import Helpers (withSecondLine)

main :: Bool -> String -> Int
main secondPuzzle = sum
                  . mapMaybe (if secondPuzzle then getGear else getPart)
                  . withNeighbours
                  . parse

getPart :: (String, [String]) -> Maybe Int
getPart (str, nbs) | isDigit (head str) && not (null nbs) = Just (read str)
                   | otherwise                            = Nothing

getGear :: (String, [String]) -> Maybe Int
getGear ("*", nbs) = case filter (isDigit . head) nbs of
                       [part1, part2] -> Just (read part1 * read part2)
                       _              -> Nothing
getGear _          = Nothing

withNeighbours :: [[(Int, String)]] -> [(String, [String])]
withNeighbours input = concat . snd . mapAccumL goLine ([]:input) $ input
  where goLine contextLines xs = ( tail contextLines
                                 , snd . mapAccumL goItem (take 3 contextLines) $ xs)
          where goItem context (pos, str) = (map shift context, (str, neighbours))

                  where neighbours     = concatMap (filter ((/='.') . head) . map snd) -- remove empty neighbours, drop positions
                                       . withSecondLine (filter ((/=pos) . fst))       -- remove self
                                       . map (takeWhile aroundUs)                      -- get all neighbours, including self
                                       $ context                                       -- around us and everything to the right

                        aroundUs (p,_) = p <= pos + length str
                        shift          = dropWhile (\(p, s) -> p + length s < pos + length str)

parse :: String -> [[(Int, String)]]
parse = map processLine . lines

processLine :: String -> [(Int, String)]
processLine = snd . mapAccumL go 0 -- count position on line
            . groupBy groupChars
  where go :: Int -> String -> (Int, (Int, String))
        go pos value = (pos + length value, (pos, value))

groupChars :: Char -> Char -> Bool
groupChars x y | x == y                 = True
               | isDigit x && isDigit y = True
groupChars _ _                          = False
