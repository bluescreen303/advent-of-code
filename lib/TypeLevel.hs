{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module TypeLevel ( Some(..)
                 , Some2(..)
                 , Singular(..)
                 , Peano(..)
                 , P
                 , Finite
                 , getFin
                 , finite
                 , SNat(..)
                 , HList(..)
                 , IList
                 , NList
                 , promoteNatList
                 , ModifyElement(..)
                 , GetEl
                 , Length
                 , ListDepth
                 , Product
                 ) where

import Numeric.Natural ( Natural )
import Data.Kind (Type)
import GHC.TypeLits (KnownNat, type (-), type (*), sameNat, natVal', SomeNat (SomeNat), someNatVal)
import Data.Type.Equality (TestEquality (testEquality), type (:~:) (Refl))
import Data.Functor.Identity (Identity)
import GHC.Exts (proxy#)
import Data.Maybe (isJust, fromJust)

data Some c where
    Some :: c t -> Some c

instance TestEquality c => Eq (Some c) where
    (Some l) == (Some r) = isJust $ testEquality l r

data Some2 c u where
    Some2 :: c t u -> Some2 c u

class Singular c x where
    singular :: c x

data Peano = Z | S Peano

type family P (n :: Natural) :: Peano where
    P 0 = 'Z
    P n = 'S (P (n - 1))

newtype Finite (n :: Natural) where
    Finite :: Natural -> Finite n

getFin :: Finite n -> Natural
getFin (Finite n) = n

finite :: forall n. KnownNat n => Natural -> Maybe (Finite n)
finite x | x < fromIntegral (natVal' @n proxy#) = Just (Finite x)
         | otherwise                            = Nothing

data SNat (n :: Natural) where
    SNat :: KnownNat n => SNat n

instance TestEquality SNat where
    testEquality a@SNat b@SNat = sameNat a b

instance KnownNat x => Singular SNat x where
    singular = SNat

data HList (c :: k -> Type) (ts :: [k]) where
    Nil  :: HList c '[]
    (:|) :: c t -> HList c ts -> HList c (t ': ts)

infixr 5 :|

type IList = HList Identity
type NList = HList SNat

instance TestEquality c => TestEquality (HList c) where
    testEquality Nil       Nil       = Just Refl
    testEquality (a :| as) (b :| bs) =
        case (testEquality a b, testEquality as bs) of
            (Just Refl, Just Refl) -> Just Refl
            _                      -> Nothing
    testEquality _         _         = Nothing

instance Singular (HList c) '[] where
    singular = Nil

instance (Singular c x, Singular (HList c) xs) => Singular (HList c) (x ': xs) where
    singular = singular :| singular

promoteNatList :: [Natural] -> Some NList
promoteNatList []     = Some Nil
promoteNatList (x:xs) = case (fromJust . someNatVal . fromIntegral $ x, promoteNatList xs) of
    (SomeNat @t _, Some rest) -> Some $ (SNat @t) :| rest

class ModifyElement (n :: Peano) ts where
    modifyElement :: (c (GetEl n ts) -> Maybe (c (GetEl n ts))) -> HList c ts -> Maybe (HList c ts)

instance ModifyElement 'Z (t ': ts) where
    modifyElement fn (x :| xs) = flip (:|) xs <$> fn x

instance ModifyElement n ts => ModifyElement ('S n) (t ': ts) where
    modifyElement fn (x :| xs) = (x :|) <$> modifyElement @n fn xs

type family Length dim where
    Length '[]       = 'Z
    Length (_ ': xs) = 'S (Length xs)

type family ListDepth t where
    ListDepth [t] = 'S (ListDepth t)
    ListDepth _   = 'Z

type family Product (xs :: [Natural]) :: Natural where
    Product '[]       = 1
    Product (x ': xs) = x * Product xs

type family GetEl (n :: Peano) (xs :: [k]) :: k where
    GetEl 'Z     (x ': _ ) = x
    GetEl ('S n) (x ': xs) = GetEl n xs

-- not exported yet.
-- fn should probably be defunctionalized

type family Map fn ts where
    Map fn '[] = '[]
    Map fn (x ': xs) = fn x ': Map fn xs

