module Year2023.Day01 where

import Data.Char (isDigit, digitToInt)
import Control.Arrow (Arrow (..))
import Data.List (tails, isPrefixOf)
import Data.Maybe (mapMaybe)
import Control.Applicative ((<|>))

main :: Bool -> String -> Int
main secondPuzzle = sum
                  . map ( uncurry (+)
                        . first (*10)
                        . lineFirstLast secondPuzzle )
                  . lines

lineFirstLast :: Bool -> String -> (Int, Int)
lineFirstLast includeSpelled = (head &&& last) . mapMaybe parser . tails
  where parser x = parseDigit x
               <|> if includeSpelled
                   then parseSpelled x
                   else Nothing

parseDigit :: String -> Maybe Int
parseDigit (x:_) | isDigit x = Just $ digitToInt x
parseDigit _                 = Nothing

parseSpelled :: String -> Maybe Int
parseSpelled x
    | "one"   `isPrefixOf` x = Just 1
    | "two"   `isPrefixOf` x = Just 2
    | "three" `isPrefixOf` x = Just 3
    | "four"  `isPrefixOf` x = Just 4
    | "five"  `isPrefixOf` x = Just 5
    | "six"   `isPrefixOf` x = Just 6
    | "seven" `isPrefixOf` x = Just 7
    | "eight" `isPrefixOf` x = Just 8
    | "nine"  `isPrefixOf` x = Just 9
    | otherwise           = Nothing
