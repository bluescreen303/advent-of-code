{-# LANGUAGE AllowAmbiguousTypes #-}
module Day_2022_08 where

import Control.Comonad (Comonad(..))
import Data.Char (digitToInt)
import Data.Monoid (Sum(..))
import Data.Foldable (Foldable(toList))
import GHC.Types (Coercible)
import GHC.Prim (coerce)

data Node a = Node { value  :: a
                   , north  :: Maybe (Node a)
                   , south  :: Maybe (Node a)
                   , west   :: Maybe (Node a)
                   , east   :: Maybe (Node a)
                   }
                   deriving Functor

instance Comonad Node where
    extract = value
    duplicate self = Node self (duplicate <$> north self)
                               (duplicate <$> south self)
                               (duplicate <$> west self)
                               (duplicate <$> east self)

newtype Northwards a = Northwards (Node a) deriving Functor
newtype Southwards a = Southwards (Node a) deriving Functor
newtype Westwards a  = Westwards  (Node a) deriving Functor
newtype Eastwards a  = Eastwards  (Node a) deriving Functor

newtype Direction f a = Direction (f a)

class Coercible Node f => Navigate f where
    step :: f a -> Maybe (Node a)

instance Navigate Northwards where step = north . coerce
instance Navigate Southwards where step = south . coerce
instance Navigate Westwards where step = west . coerce
instance Navigate Eastwards where step = east . coerce

instance Navigate f => Foldable (Direction f) where
  foldMap :: forall a m. Monoid m => (a -> m) -> Direction f a -> m
  foldMap fn dir =
        let self = fn . value . coerce $ dir
        in maybe self ((self <>) . foldMap @(Direction f) fn . coerce @(Node a))
                 (step @f (coerce dir))

instance Foldable Node where
  foldMap :: forall m a. Monoid m => (a -> m) -> Node a -> m
  foldMap fn = foldMap @(Direction Southwards) (foldMap @(Direction Eastwards) fn . coerce @(Node a)) . coerce . duplicate

-- quad linked list :)
grid :: [[a]] -> Maybe (Node a)
grid = go Nothing
    where go _    []      = Nothing
          go above (x:xs) = self
               where self = line Nothing above (go self xs) x

          line _    _     _     []     = Nothing
          line left above below (x:xs) = self
              where self = Just $ Node x above
                                         below
                                         left
                                         (line self (above >>= east) (below >>= east) xs)

ungrid :: forall a. Node a -> [[a]]
ungrid = foldMap @(Direction Southwards) ((:[]) . foldMap @(Direction Eastwards) (:[]) . coerce @(Node a)) . coerce . duplicate

look :: forall b a. Navigate b => Node a -> [a]
look = tail . toList @(Direction b) . coerce

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
