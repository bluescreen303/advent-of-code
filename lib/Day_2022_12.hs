{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Day_2022_12 where

import Grid
import Control.Monad ((>=>))
import Control.Comonad (Comonad(..))
import Data.Char (ord)
import Data.Foldable (find)
import Data.Traversable (mapAccumR)
import Data.Maybe (mapMaybe, listToMaybe)
import GHC.Char (chr)
import Control.Arrow (second)
import GHC.TypeLits (KnownNat)
import Data.Word (Word8)
import TypeLevel (HList(..))
import Data.Functor.Identity (Identity(..))

data Location = Normal Word8
              | StartLoc
              | EndLoc
              deriving Eq

height :: Location -> Word8
height StartLoc   = 0
height EndLoc     = 25
height (Normal x) = x

instance Show Location where
      show StartLoc   = "S"
      show EndLoc     = "E"
      show (Normal x) = [chr $ ord 'a' + fromIntegral x]

instance (KnownNat x, KnownNat y) => Show (Grid [y, x] Location) where
      show = unlines . map (concatMap show) . unGrid

parse :: String -> [[Location]]
parse = map (map locationFromChar) . lines
    where locationFromChar 'S'     = StartLoc
          locationFromChar 'E'     = EndLoc
          locationFromChar x
            | 'a' <= x && x <= 'z' = Normal (fromIntegral $ ord x - ord 'a')
            | otherwise            = error $ "invalid input char: " ++ [x]

data Tracking = Unvisited | Here | WentNorth | WentSouth | WentWest | WentEast
              deriving Eq

instance Show Tracking where
      show Unvisited   = "."
      show Here        = "#"
      show WentNorth   = "^"
      show WentSouth   = "v"
      show WentWest    = "<"
      show WentEast    = ">"

instance (KnownNat x, KnownNat y) => Show (Grid [y, x] Tracking) where
      show = unlines . map (concatMap show) . unGrid

type WorldState           x y = Focus (Layers '[      Location]) [y, x] Tracking
type WorldStateWithVisits x y = Focus (Layers '[Bool, Location]) [y, x] Tracking

addVisited :: Grid '[y, x] Bool -> WorldState x y -> WorldStateWithVisits x y
addVisited vis (Focus (Layers (t :| l :| Nil)) f) = Focus (Layers (t :| vis :| l :| Nil)) f

removeVisited :: WorldStateWithVisits x y -> WorldState x y
removeVisited (Focus (Layers (t :| _ :| l :| Nil)) f) = Focus (Layers (t :| l :| Nil)) f

toVisited :: Focus (Layers ts) dim Tracking -> Grid dim Bool
toVisited = fmap isVisited . topLayer . world
      where isVisited Unvisited = False
            isVisited _         = True

liftVisited :: (WorldStateWithVisits x y -> Maybe (WorldStateWithVisits x y)) -> WorldState x y -> Maybe (WorldState x y)
liftVisited fn x = fmap removeVisited . fn . addVisited (toVisited x) $ x

isLowest :: Location -> Bool
isLowest l = height l == 0

isStartPos :: Location -> Bool
isStartPos StartLoc = True
isStartPos _        = False


trackAll :: (KnownNat x, KnownNat y) => (Location -> Bool) -> Grid [y, x] Location -> [WorldState x y]
trackAll p =
      foldMap (\x -> [mapFocus (addLayer (fmap startLoc . world $ x) . toLayers) x | p (value x)])
      . world
      . duplicate
      . mkFocus
  where startLoc x | p x       = Here
                   | otherwise = Unvisited

track :: (KnownNat x, KnownNat y) => Grid [y, x] Location -> Maybe (WorldState x y)
track = listToMaybe . trackAll isStartPos

step :: (KnownNat x, KnownNat y)
     => (forall ts a. Focus (Layers ts) [y, x] a -> Maybe (Focus (Layers ts) [y, x] a))
     -> Tracking
     -> WorldStateWithVisits x y
     -> Maybe (WorldStateWithVisits x y)
step dir t node = case values node of
      Identity Here :| _ :| Identity l1 :| Nil -> (dir . setValue t $ node) >>= \next -> case values next of
            Identity Unvisited :| Identity False :| Identity l2 :| Nil
                | height l2 <= height l1 + 1 -> Just $ setValue Here next
            _                                -> Nothing
      _                                      -> Nothing

stepN, stepS, stepW, stepE :: (KnownNat x, KnownNat y) => WorldStateWithVisits x y -> Maybe (WorldStateWithVisits x y)
stepN = step (move north) WentNorth
stepS = step (move south) WentSouth
stepW = step (move west)  WentWest
stepE = step (move east)  WentEast

data Progress sx = Walking (Grid sx Bool, [Focus (Layers '[Location]) sx Tracking])
                 | Found   (Focus (Layers '[Location]) sx Tracking)
                 | NotFound

stepEverywhere :: forall x y. (KnownNat x, KnownNat y) => Grid [y,x] Bool -> [WorldState x y] -> Progress [y, x]
stepEverywhere _       [] = NotFound
stepEverywhere visited ns = maybe (Walking options) Found $ find end (snd options)
    where options = second concat . mapAccumR withVisited visited $ ns
          withVisited vis w =
            let opts = map removeVisited . mapMaybe ($ addVisited vis w) $ [stepN, stepS, stepW, stepE]
            in (foldr (Grid.zipWith (||) . toVisited) vis opts, opts)

          end :: WorldState x y -> Bool
          end x   = case values x of
            Identity Here :| Identity EndLoc :| Nil -> True
            _                                       -> False


solveAll :: (KnownNat x, KnownNat y) => [WorldState x y] -> Maybe (Int, Grid [y, x] Tracking)
solveAll [] = Nothing
solveAll nn = go 1 nn (foldr1 (Grid.zipWith (||)) $ map toVisited nn)
    where go i ns v = case stepEverywhere v ns of
              Walking (v', ns') -> go (i+1) ns' v'
              Found s           -> Just (i, topLayer $ world s)
              NotFound          -> Nothing

solve :: (KnownNat x, KnownNat y) => WorldState x y -> Maybe (Int, Grid [y, x] Tracking)
solve n = solveAll [n]

main :: String -> Maybe Int
main = fmap fst . ((mkSomeGrid @2 . parse) >=> grid2D (solveAll . trackAll isStartPos >=> (return . second toSomeGrid)))

mainAll :: String -> Maybe Int
mainAll = fmap fst . ((mkSomeGrid @2 . parse) >=> grid2D (solveAll . trackAll isLowest >=> return . second toSomeGrid))

-- l1 :: Grid [2,2] Int
-- l1 = fromJust $ mkGrid [[1,2],[3,4]]

-- l2 :: Grid [2,2] Float
-- l2 = fromJust $ mkGrid [[1.1,2.2],[3.3,4.4]]

-- l3 :: Grid [2,2] Char
-- l3 = fromJust $ mkGrid [['a', 'b'],['c', 'd']]

-- ll :: Layers [Float, Char] [2,2] Int
-- ll = Layers (l1 :| l2 :| l3 :| Nil)

-- foo :: Focus (Layers '[Float, Char]) [2, 2] Int
-- foo = mkFocus ll

-- bar :: Focus (Layers '[Float, Char]) [2,2] String
-- bar = extend go foo
--   where go :: Focus (Layers '[Float, Char]) '[2,2] Int -> String
--         go x = let Identity i :| Identity f :| Identity c :| Nil = values x
--                in intercalate "," [show i, show f, show c]

-- result :: Grid '[2, 2] String
-- result = case world bar of
--       (Layers (g :| _)) -> g