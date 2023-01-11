{-# LANGUAGE AllowAmbiguousTypes #-}
module Day_2022_08 where

import Control.Comonad (Comonad(..))
import Data.Char (digitToInt)
import Data.Monoid (Sum(..))
import Grid

visibleFrom :: forall b a. (Ord a, Navigate b) => Node a -> Bool
visibleFrom me = all (< value me) (look @b me)

-- visibleFrom' :: forall b a. (Ord a, Navigate b) => Node a -> Bool
-- visibleFrom' me = maybe True (foldr (flip (&&) . (< value me)) True) next
--     where next :: Maybe (Direction b a)
--           next = fmap coerce . step @b @a . coerce $ me

visible :: Ord a => Node a -> Bool
visible me = visibleFrom @Northwards me
          || visibleFrom @Southwards me
          || visibleFrom @Westwards  me
          || visibleFrom @Eastwards  me

countVisible :: Ord a => Node a -> Int
countVisible = getSum
             . foldMap (\q -> if q then Sum 1 else Sum 0)
             . extend visible

countTrees :: forall b a. (Ord a, Navigate b) => Node a -> Int
countTrees me = let (ok, rest) = span (< value me) . look @b $ me
                in length ok + if null rest then 0 else 1

scenicScore :: Ord a => Node a -> Int
scenicScore me = countTrees @Northwards me
               * countTrees @Southwards me
               * countTrees @Westwards  me
               * countTrees @Eastwards  me

bestScenicScore :: Ord a => Node a -> Int
bestScenicScore = maximum
                . extend scenicScore

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

main :: String -> Maybe (Int, Int)
main = fmap ((,) <$> countVisible <*> bestScenicScore) . grid . parse
