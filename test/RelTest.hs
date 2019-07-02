{-# LANGUAGE TemplateHaskell #-}
module RelTest where

import Test.QuickCheck
import Test.QuickCheck.All
-- https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html
-- https://www.tfp2019.org/resources/tfp2019-how-to-specify-it.pdf

import ListRel
import Data.Word
import Algebra.Lattice
--dummy stuff


prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs

prop_commutativeAdd :: Int -> Int -> Bool
prop_commutativeAdd x y = x + y == y + x

prop_ordering :: Bool -> Bool
prop_ordering x  = x == x

type R1 = Rel Bool Ordering



prop_indirect_eq :: R1 -> R1 -> R1 -> Bool
prop_indirect_eq x y z =  (y ~~ x) `implies` ((z <~ x) == (z <~ y)) -- hmm. quickcheck is never gonna find y ~~ x 

prop_ridleft :: Rel Bool Ordering -> Bool
prop_ridleft x = (rid <<< x) ~~ x

prop_ridright :: Rel Bool Ordering  -> Bool
prop_ridright x = (x <<< rid) ~~ x

prop_meet :: R1 -> R1  -> Bool
prop_meet x y = (x /\ y) <~ x

prop_meet' :: R1 -> R1  -> Bool
prop_meet' x y = (x /\ y) <~ y
{-
prop_meet_assoc
prop_meet_comm

-}
prop_join_univ :: R1 -> R1 -> R1 -> Bool
prop_join_univ x y z = ((x \/ y) <~ z) == ((x <~ z) && (y <~ z))

prop_join :: R1 -> R1  -> Bool
prop_join x y = y <~ (x \/ y) 


prop_meet_univ :: R1 -> R1 -> R1 -> Bool
prop_meet_univ x y z = (z <~ (x /\ y)) == ((z <~ x) && (z <~ y))

prop_top :: R1 -> Bool
prop_top x = x <~ top

prop_bottom :: R1 -> Bool
prop_bottom x = bottom <~ x

prop_bottom' :: R1 -> Bool
prop_bottom' x = (x <<< bottom) ~~ (bottom :: R1)

prop_trans_iso :: Rel (Bool, Ordering) Word8 -> Bool
prop_trans_iso x = untrans (trans x) == x

prop_rdiv :: Rel Bool Ordering -> Rel Word8 Ordering -> Bool
prop_rdiv g j = (j <<< (rdiv g j)) <~ g

prop_con :: R1 -> Bool
prop_con x = con (con x) ~~ x

prop_rdiv' :: Rel Bool Word8 -> Rel Bool Ordering -> Rel Word8 Ordering -> Bool
prop_rdiv' x g j = (x <~ (rdiv g j)) == ((j <<< x) <~ g) 

{- prop_rdiv'' :: Rel Bool Ordering -> Rel Bool Ordering -> Rel Ordering Ordering -> Bool
prop_rdiv'' x g j = (x <~ (rdiv g j)) == ((x <<< j) <~ g) 
-}
{-
-- monotonicity properties

-}

-- very strange. But this is what they told me to do.
return []
mycheck = $quickCheckAll