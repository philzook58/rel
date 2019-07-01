{-# LANGUAGE TypeApplications, DataKinds, ScopedTypeVariables, PolyKinds, GeneralizedNewtypeDeriving #-}
module Main where

import Data.Proxy
import ListRel
import GHC.TypeLits 

newtype FinNat n = FinNat Integer deriving (Show, Enum, Eq, Ord, Num)

-- valFin :: forall n. KnownNat n => FinNat n
-- valFin = FinNat $ valNat (Proxy @n)

instance KnownNat n => Bounded (FinNat n) where
    minBound = 0
    maxBound = FinNat $ natVal (Proxy @n)

cap :: forall n. KnownNat n => FinNat n -> FinNat n
cap z | z < 0 = 0
      | z < n = z
      | otherwise = n where n = maxBound @(FinNat n)
{-
instance KnownNat n => Num (FinNat n) where
    (FinNat x) + (FinNat y) = cap $ FinNat (x + y)
    (FinNat x) * (FinNat y) = cap $ FinNat (x * y)
    (FinNat x) - (FinNat y) = cap $ FinNat (x - y)
    fromInteger x = cap $ FinNat x
    abs = id
    signum (FinNat x) = FinNat (signum x)
-}

type Fifty = FinNat 50
plus' :: Fifty -> Rel Fifty Fifty
plus' x = [(y, x + y) | y <- enumAll, x + y <= 50 ] -- map liftFun (+)

sub' x = con (plus' x)

plus :: Rel (Fifty,Fifty) Fifty
plus = tabulate2 (+)

sub = con (trans plus)
-- rcompose  y(plus x) \rsub rid <=> 
-- test x = rdiv rid (plus x)

-- plusrel :: Rel (Fifty, Fifty) Fifty
--plusrel = liftFun2 (+)
{-

data Galois a b c d = Galois {f :: Rel a b -> Rel c d, g :: Rel c d -> Rel a b} -- f X <= Y <-> X <= g Y 

gcompose :: {}
Galois (compose (plus x)) (rdiv (plus x))
-- the galois connection from relational composition and division
gengalois :: Rel a b -> Galois
gengalois r = Galois (compose r) (rdiv r)
-}

main :: IO ()
main = print "yo"
