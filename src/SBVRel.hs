module SBVRel where

import Data.SBV

-- do I want SBV in there? Do I want it wrapping a and b instead?
type SBVRel a b = Symbolic (SBV a, SBV b)

type SBVRel' a b = a -> b -> SBool -- as predicates
-- do b <- free_; \a c -> (f a b) .&& (g b c)
{-
rid :: (SymVal a, SymVal b) => SBVRel a b
rid = do 
      x <- free_
      y <- free_
      return (x,y)

rcompose :: SBVRel b c -> SBVRel a b -> SBVRel a c
rcompose f g = do 
                (b,c) <-  f
                (a,b') <- g
                constrain $ b .== b'
                return (a,c)

rSub :: SBVRel a b -> SBVRel a b -> SBool
rSub :: SBVRel a b -> SBVRel a b -> Bool
rSub f g = forall \(a,b) -> (f a b) .==> (g a b)
--rSub f g =     
-}
-- p1 = prove $ \a b -> (rcompose rid rid) .== rid