{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module Hog where
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import ListRel hiding ((===))

-- a delapidated attempt at using hedgehog
{-
prop_reverse :: Property
prop_reverse =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    reverse (reverse xs) === xs

prop_rid_left :: Property
prop_rid_left = property $ do
                (r :: Rel Bool Ordering) <- forAll $ Gen.list (Range.linear 0 10) $ (,) <$> Gen.enumBounded <*> Gen.enumBounded
                assert ((rcompose rid r) `rEq` r)

prop_rid_right :: Property
prop_rid_right = simple (\r -> ((rcompose r rid) `rEq` r))


simplerel ::  (MonadGen m) => m [(Bool, Ordering)]
simplerel = Gen.list (Range.linear 0 10) $ (,) <$> Gen.enumBounded <*> Gen.enumBounded

prop_universal_meet :: Property
prop_universal_meet = property $ do 
    r1 <- forAll simplerel
    r2 <- forAll simplerel
    assert ((r1 /\ r2) `rSub` r1)

prop_universal_meet_2 :: Property
prop_universal_meet_2 = property $ do 
    r1 <- forAll simplerel
    r2 <- forAll simplerel
    assert ((r1 /\ r2) `rSub` r2)

prop_universal_join_2 :: Property
prop_universal_join_2 = property $ do 
    r1 <- forAll simplerel
    r2 <- forAll simplerel
    assert (r2 `rSub` (r1 \/ r2) )

prop_bottom = property $ do
    r <- forAll simplerel
    assert (bottom `rSub` r)

prop_top = property $ do
    r <- forAll simplerel
    assert (r `rSub` top)

prop_iso_search = property $ do
    r <- forAll simplerel
    assert ((tabulateSearch (searchRel r)) `rEq` r)
-}
{-

other properties
top meet r == r
bottom meet r = bottom
bottom join r = r
top join r = top

indirect equality?

guard (y `mod` 2 == (1 :: Int))
preconditions

(rdiv g j) . j `rSub` g

forall x, filter x . j `rsub` g
x `rsub` (rdib g j)

make greatest lower bound property literal
given r sub p
given r sub q
assert r sub (p meet q)
-}


simple prop = property $ do
    (r :: Rel Bool Ordering) <- forAll $ Gen.list (Range.linear 0 10) $ (,) <$> Gen.enumBounded <*> Gen.enumBounded
    assert (prop r)