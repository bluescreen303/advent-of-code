{-# LANGUAGE AllowAmbiguousTypes #-}
module Grid ( Node
            , Direction
            , Northwards
            , Southwards
            , Westwards
            , Eastwards
            , Navigate
            , grid
            , ungrid
            , value
            , setValue
            , look
            , north
            , south
            , west
            , east
            ) where

import Control.Comonad (Comonad(..))
import Data.Maybe (fromJust)
import Data.Foldable (Foldable(toList))
import GHC.Types (Coercible)
import GHC.Prim (coerce)
import Data.Vector (Vector, (!), (!?), (//))
import qualified Data.Vector as V

newtype Grid a = Grid { unGrid :: Vector (Vector a) } deriving (Functor, Foldable)

toGrid :: [[a]] -> Grid a
toGrid = Grid . V.fromList . map V.fromList

fromGrid :: Grid a -> [[a]]
fromGrid (Grid v) = map V.toList . V.toList $ v

data Node a = Node { fgrid :: Grid a
                   , focus :: (Int, Int) }
              deriving (Functor, Foldable)

grid :: [[a]] -> Maybe (Node a)
grid = moveTo (0,0) . flip Node (0, 0) . toGrid

ungrid :: Node a -> [[a]]
ungrid = fromGrid . fgrid

moveTo :: (Int, Int) -> Node a -> Maybe (Node a)
moveTo (x, y) (Node (Grid v) _) = do
    l <- v !? y
    _ <- l !? x
    return (Node (Grid v) (x, y))

north :: Node a -> Maybe (Node a)
north f@(Node _ (x, y)) = moveTo (x, y-1) f

south :: Node a -> Maybe (Node a)
south f@(Node _ (x, y)) = moveTo (x, y+1) f

west :: Node a -> Maybe (Node a)
west f@(Node _ (x, y)) = moveTo (x-1, y) f

east :: Node a -> Maybe (Node a)
east f@(Node _ (x, y)) = moveTo (x+1, y) f

value :: Node a -> a
value  (Node (Grid v) (x, y)) = v ! y ! x

setValue :: a -> Node a -> Node a
setValue s (Node (Grid v) (x, y)) = Node (Grid vert) (x, y)
    where vert  = v // [(y, horiz)]
          horiz = (v ! y) // [(x, s)]

newtype Northwards a = Northwards (Node a) deriving Functor
newtype Southwards a = Southwards (Node a) deriving Functor
newtype Westwards a  = Westwards  (Node a) deriving Functor
newtype Eastwards a  = Eastwards  (Node a) deriving Functor

class Coercible Node f => Navigate f where
    step :: f a -> Maybe (Node a)
    walk :: f a -> Node a
    walk dir = case step dir of
                 Nothing -> coerce dir
                 Just x  -> walk (coerce @_ @(f _) x)

instance Navigate Northwards where step = north . coerce
instance Navigate Southwards where step = south . coerce
instance Navigate Westwards where step = west . coerce
instance Navigate Eastwards where step = east . coerce

newtype Direction f a = Direction (f a)

instance Navigate f => Foldable (Direction f) where
  foldMap :: forall a m. Monoid m => (a -> m) -> Direction f a -> m
  foldMap fn dir =
        let self = fn . value . coerce $ dir
        in maybe self ((self <>) . foldMap @(Direction f) fn . coerce @(Node a))
                 (step @f (coerce dir))

look :: forall b a. Navigate b => Node a -> [a]
look = tail . toList @(Direction b) . coerce

instance Comonad Node where
    extract        = value
    duplicate self = Node (Grid . V.imap vertical . unGrid . fgrid $ self) (focus self)
      where vertical   y     = V.imap (horizontal y)
            horizontal y x _ = fromJust (moveTo (x, y) self)
