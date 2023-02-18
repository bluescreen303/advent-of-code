{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Grid ( Grid, mkGrid, unGrid, append, Grid.zipWith
            , SomeGrid(..), mkSomeGrid, toSomeGrid
            , grid2D, grid3D, north, south, east, west
            , Layers(..), topLayer, toLayers, addLayer
            , Focus(..), mkFocus, mapFocus, value, values, setValue
            , move, walk, look
            ) where

import Control.Comonad (Comonad(..))
import Data.Type.Equality (TestEquality(testEquality))
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Numeric.Natural ( Natural )
import GHC.Natural (minusNaturalMaybe)
import GHC.TypeLits (KnownNat, type (+))
import TypeLevel
import Sized (Indexed(..), Sized (..), Index, InputForm, Dim, pack, modIndex)

newtype Grid (sx :: [Natural]) t = Grid (Vector t)
    deriving (Functor, Foldable, Traversable)

mkGrid :: forall sx t. (Singular NList sx, Dim (Length sx))
       => InputForm (Length sx) t -> Maybe (Grid sx t)
mkGrid input = do (Some d, v) <- pack @(Length sx) @t input
                  _ <- testEquality (singular :: NList sx) d
                  return $ Grid v

unGrid :: forall sx t. Sized sx
       => Grid sx t -> InputForm (Length sx) t
unGrid (Grid v) = inputForm @sx v

instance Indexed Grid where
    type Get Grid t = t
    get :: forall sx t. Sized sx => Index sx -> Grid sx t -> t
    get idx (Grid v) = (v !) . fromIntegral . slice @sx $ idx
    getAll = get
    set :: forall sx t. Sized sx => t -> Index sx -> Grid sx t -> Grid sx t
    set val idx (Grid v) = Grid (v V.// [(fromIntegral $ slice @sx idx, val)])
    imap fn = Grid . V.imap (fn . unslice . fromIntegral) . toVec
      where toVec (Grid v) = v

-- utils

append :: Grid (x ': xs) t -> Grid (y ': xs) t -> Grid (x + y ': xs) t
append (Grid v1) (Grid v2) = Grid $ v1 V.++ v2

zipWith :: (a -> b -> c) -> Grid sx a -> Grid sx b -> Grid sx c
zipWith fn (Grid v1) (Grid v2) = Grid (V.zipWith fn v1 v2)

-- grids of size only known at runtime

data SomeGrid t where
    SomeGrid :: Grid sx t -> NList sx -> SomeGrid t

toSomeGrid :: Singular NList sx => Grid sx t -> SomeGrid t
toSomeGrid g = SomeGrid g singular

mkSomeGrid :: forall n t. Dim (P n) => InputForm (P n) t -> Maybe (SomeGrid t)
mkSomeGrid input = do (Some dim, v) <- pack @(P n) @t input
                      return $ SomeGrid (Grid v) dim

-- 2D & 3D utils

grid2D :: (forall x y. (KnownNat x, KnownNat y) => Grid [y, x] t -> r) -> SomeGrid t -> r
grid2D fn (SomeGrid g (SNat :| SNat :| Nil)) = fn g
grid2D _  _                                  = undefined

grid3D :: (forall x y z. (KnownNat x, KnownNat y, KnownNat z) => Grid [z, y, x] t -> r) -> SomeGrid t -> r
grid3D fn (SomeGrid g (SNat :| SNat :| SNat :| Nil)) = fn g
grid3D _  _                                          = undefined

north :: KnownNat y => Index [y, x] -> Maybe (Index [y, x])
north = modIndex @0 (`minusNaturalMaybe` 1)

south :: KnownNat y => Index [y, x] -> Maybe (Index [y, x])
south = modIndex @0 (pure . (+1))

east :: KnownNat x => Index [y, x] -> Maybe (Index [y, x])
east = modIndex @1 (pure . (+1))

west :: KnownNat x => Index [y, x] -> Maybe (Index [y, x])
west = modIndex @1 (`minusNaturalMaybe` 1)

-- instance (KnownNat x, KnownNat y, Show t) => Show (Grid [y, x] t) where
--       show = unlines . map (concatMap show) . unGrid

--- layered

-- a layered grid is a grid that has a writable top layer (grid)
-- but by using `getAll`, it can view the layers below.
-- This can be put to use via Focus and via Comonadic mapping

type Layered (dim :: [Natural]) = HList (Grid dim)

-- non-empty stack of layers

data Layers ts dim t where
    Layers :: { unLayers :: Layered dim (t ': ts) } -> Layers ts dim t

instance Functor (Layers ts dim) where
    fmap fn (Layers (g :| gs)) = Layers (fmap fn g :| gs)

instance Foldable (Layers ts dim) where
    foldr fn z (Layers (g :| _)) = foldr fn z g

toLayers :: Grid dim t -> Layers '[] dim t
toLayers g = Layers (g :| Nil)

addLayer :: Grid dim x -> Layers ts dim t -> Layers (t ': ts) dim x
addLayer g (Layers ls) = Layers (g :| ls)

topLayer :: Layers ts dim t -> Grid dim t
topLayer (Layers (g :| _)) = g

instance Indexed (Layers ts) where
  type Get (Layers ts) t = IList (t ': ts)
  get     i (Layers (g :| _)) = get i g
  getAll :: forall sx t. Sized sx => Index sx -> Layers ts sx t -> Get (Layers ts) t
  getAll  i (Layers l)        = go l
    where go :: Layered sx ls -> IList ls
          go Nil       = Nil
          go (g :| gs) = get i g ::| go gs
  set v i (Layers (g :| gs))  = Layers (set v i g :| gs)
  imap :: Sized sx => (Index sx -> t -> u) -> Layers ts sx t -> Layers ts sx u
  imap fn (Layers (g :| gs))  = Layers (imap fn g :| gs)

--- focus

data Focus w sx t = Focus { world :: w sx t
                          , focus :: Index sx
                          } deriving Functor

mapFocus :: (w1 sx a -> w2 sx b) -> Focus w1 sx a -> Focus w2 sx b
mapFocus fn (Focus w f) = Focus (fn w) f

instance (Indexed w, Sized sx, Functor (w sx)) => Comonad (Focus w sx) where
    extract = value
    duplicate (Focus w f) = Focus (imap go w) f
        where go i _ = Focus w i

mkFocus :: Sized sx => w sx t -> Focus w sx t
mkFocus = flip Focus origin

value :: (Indexed w, Sized sx) => Focus w sx t -> t
value f = get (focus f) (world f)

values :: (Indexed w, Sized sx) => Focus w sx t -> Get w t
values f = getAll (focus f) (world f)

setValue :: (Indexed w, Sized sx) => t -> Focus w sx t -> Focus w sx t
setValue x (Focus w f) = Focus (set x f w) f

move :: (Index sx -> Maybe (Index sx)) -> Focus w sx t -> Maybe (Focus w sx t)
move fn (Focus w f) = Focus w <$> fn f

walk :: (Indexed w, Sized sx) => (Index sx -> Maybe (Index sx)) -> Focus w sx t -> [t]
walk fn me = value me : maybe [] (walk fn) (move fn me)

look :: (Indexed w, Sized sx) => (Index sx -> Maybe (Index sx)) -> Focus w sx t -> [t]
look fn me = tail (walk fn me)


-- tests & helpers

-- testSomeGrid :: Show t => SomeGrid t -> String
-- testSomeGrid (SomeGrid (Grid v) dim) = show (v, nListToList dim)

-- nListToList :: NList ns -> [Natural]
-- nListToList Nil = []
-- nListToList (x@SNat :| xs) = fromIntegral (natVal x) : nListToList xs

-- finListToList :: HList Finite ns -> [Natural]
-- finListToList Nil = []
-- finListToList (x :| xs) = getFin x : finListToList xs

-- test0 :: Grid '[] Int
-- test0 = fromJust $ mkGrid 42

-- test1 :: Grid '[3] Int
-- test1 = fromJust $ mkGrid [5,4,3]

-- tst1 :: Int
-- tst1 = fromJust $ get' [1] test1

-- test2 :: Grid [3,4] Int
-- test2 = fromJust $ mkGrid [[1,2,3,11],[4,5,6,12],[7,8,9,13]]

-- tst2 :: Int
-- tst2 = fromJust $ get' [1, 3] test2

-- test2a :: SomeGrid Int
-- test2a = fromJust $ mkSomeGrid @2 [[1,2,3,11],[4,5,6,12],[7,8,9,13]]

-- test3 :: Grid [3,4,5] Int
-- test3 = fromJust $ mkGrid [[[001,002,003,004,005],[006,007,008,009,010],[011,012,013,014,015],[016,017,018,019,020]]
--                           ,[[101,102,103,104,105],[106,107,108,109,110],[111,112,113,114,115],[116,117,118,119,120]]
--                           ,[[201,202,203,204,205],[206,207,208,209,210],[211,212,213,214,215],[216,217,218,219,220]]]

-- test4 :: Layered [3,4] [Int, String]
-- test4 = test2 :| fmap show test2 :| Nil

