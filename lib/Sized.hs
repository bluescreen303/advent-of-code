{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Sized ( InputForm
             , Dim
             , pack
             , Index
             , modIndex
             , Sized(..)
             , Indexed(..)
             , get'
             ) where

import Control.Monad ((<=<))
import Data.Functor.Identity (Identity(..))
import Data.List (nub)
import Data.Maybe (fromJust)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import GHC.Exts (proxy#)
import GHC.Natural (Natural)
import GHC.TypeLits (KnownNat, natVal', SomeNat (..), someNatVal)
import TypeLevel (Length, Product, HList (..), Finite, Peano (..), getFin, finite, Some (..), NList, SNat (..), ModifyElement, P, GetEl, modifyElement)

type family InputForm n t where
    InputForm 'Z     t = t
    InputForm ('S n) t = [InputForm n t]

type family ListForm n t = result | result -> n t where
    ListForm 'Z     t = Identity t
    ListForm ('S n) t = [ListForm n t]

class Dim (n :: Peano) where
    prep    :: InputForm n t -> ListForm n t
    sizes   :: ListForm n t  -> Maybe (Some NList)
    pack'   :: ListForm n t  -> Maybe (Some NList, Vector t)

pack :: forall n t. Dim n => InputForm n t -> Maybe (Some NList, Vector t)
pack = pack' @n . prep @n

instance Dim 'Z where
    prep        = Identity
    sizes _     = Just (Some Nil)
    pack' input = (, V.singleton (runIdentity input)) <$> sizes input

instance Dim n => Dim ('S n) where
    prep        = map prep
    sizes input = ncons <$> self <*> rest
        where ncons (SomeNat @self' _) (Some rest') = Some (SNat @self' :| rest')
              self = someNatVal . fromIntegral . length $ input
              rest = case nub . map (sizes @n) $ input of
                       [x] -> x
                       _   -> Nothing
    pack' input = (,) <$> sizes input <*> val
        where val = fmap (foldr1 (V.++) . map snd) . traverse pack' $ input


type Index sx = HList Finite sx

modIndex :: forall n sx. (ModifyElement (P n) sx, KnownNat (GetEl (P n) sx))
         => (Natural -> Maybe Natural)
         -> Index sx -> Maybe (Index sx)
modIndex fn = modifyElement @(P n) (finite <=< fn . getFin)

class Sized (sx :: [Natural]) where
    slice     :: Index sx  -> Natural
    unslice   :: Natural   -> Index sx
    index     :: [Natural] -> Maybe (Index sx)
    inputForm :: Vector t  -> InputForm (Length sx) t
    origin    :: Index sx

instance Sized '[] where
    inputForm v = v ! 0
    slice _     = 0
    unslice _   = Nil
    index []    = Just Nil
    index _     = Nothing
    origin      = Nil

instance (KnownNat s, KnownNat (Product sx), Sized sx) => Sized (s ': sx) where
    inputForm    = fmap (inputForm @sx) . splitPer (prodSX @sx)

    slice (finX :| xs) = fromIntegral (getFin finX) * (prodSX @sx) + slice xs

    unslice n    = fromJust (finite q) :| unslice r
        where (q, r) = quotRem n (prodSX @sx)

    index []     = Nothing
    index (x:xs) = (:|) <$> finite @s x <*> index @sx xs

    origin       = fromJust (finite 0) :| origin

class Indexed f where
    type Get f t
    get    :: forall sx t.   Sized sx =>             Index sx -> f sx t -> t
    getAll :: forall sx t.   Sized sx =>             Index sx -> f sx t -> Get f t
    set    :: forall sx t.   Sized sx =>        t -> Index sx -> f sx t -> f sx t
    imap   :: forall sx t u. Sized sx => (Index sx -> t -> u) -> f sx t -> f sx u

get' :: forall f sx t. (Indexed f, Sized sx) => [Natural] -> f sx t -> Maybe t
get' idx f = flip get f <$> index idx


-- utils

prodSX :: forall sx r. (KnownNat (Product sx), Num r) => r
prodSX = fromIntegral $ natVal' @(Product sx) proxy#

splitPer :: Int -> Vector a -> [Vector a]
splitPer n xs = let (this, that) = V.splitAt n xs
                in this : if V.null that then [] else splitPer n that
