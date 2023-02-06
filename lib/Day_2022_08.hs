{-# LANGUAGE DataKinds #-}
module Day_2022_08 where

import Control.Comonad (Comonad(..))
import Data.Char (digitToInt)
import Data.Monoid (Sum(..))
import GHC.TypeLits (KnownNat)
import Sized (Sized, Index)
import Grid

visibleFrom :: (Sized sx, Ord t) => (Index sx -> Maybe (Index sx)) -> Focus Grid sx t -> Bool
visibleFrom dir me = all (< value me) (look dir me)

visible :: (KnownNat x, KnownNat y, Ord t) => Focus Grid [y, x] t -> Bool
visible me = visibleFrom north me
          || visibleFrom south me
          || visibleFrom west  me
          || visibleFrom east  me

countVisible :: (KnownNat x, KnownNat y, Ord t) => Grid [y, x] t -> Int
countVisible = getSum
             . foldMap (\q -> if q then Sum 1 else Sum 0)
             . world
             . extend visible
             . mkFocus

countTrees :: (Sized sx, Ord t) => (Index sx -> Maybe (Index sx)) -> Focus Grid sx t -> Int
countTrees dir me = let (ok, rest) = span (< value me) . look dir $ me
                    in length ok + if null rest then 0 else 1

scenicScore :: (KnownNat x, KnownNat y, Ord t) => Focus Grid [y, x] t -> Int
scenicScore me = countTrees north me
               * countTrees south me
               * countTrees west  me
               * countTrees east  me

bestScenicScore :: (KnownNat x, KnownNat y, Ord t) => Grid [y, x] t -> Int
bestScenicScore = maximum
                . world
                . extend scenicScore
                . mkFocus

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

main :: String -> Maybe (Int, Int)
main = fmap (grid2D ((,) <$> countVisible <*> bestScenicScore)) . mkSomeGrid @2 . parse
