{-# LANGUAGE ConstraintKinds, TypeApplications, 
ScopedTypeVariables, AllowAmbiguousTypes, TypeSynonymInstances, FlexibleInstances, 
InstanceSigs, FlexibleContexts #-}
module ListRel where

import Data.List (nub)
import Algebra.Lattice
import Data.Foldable (toList)
type Rel a b = [(a,b)]


type BEnum a = (Enum a, Bounded a) 

enumAll :: (BEnum a) => [a]
enumAll = [minBound .. maxBound]

{- finite enumation :: Enum a -> Int -> [a] = take n [0 .. ] 
Could be useful for inductive data types?
-}

tabulate :: (BEnum a) => (a -> b) -> Rel a b
tabulate f = [(x, f x) | x <- enumAll]

tabulate2 :: (BEnum a, BEnum b) => (a -> b -> c) -> Rel (a,b) c
tabulate2 f = [((x,y), f x y) | x <- enumAll, y <- enumAll]

rcompose :: Eq b => Rel b c -> Rel a b -> Rel a c
rcompose xs ys = [ (a,c)  | (a, b) <- ys, (b', c) <- xs, b' == b]

x <<< y = rcompose x y

rid :: (Enum a, Bounded a) => Rel a a
rid = tabulate id -- diagRel enumAll

diagRel :: [a] -> Rel a a
diagRel = map dup where dup x = (x,x)

leftSet :: Eq a => Rel a b -> [a]
leftSet = nub . (map fst)

rightSet :: Eq b => Rel a b -> [b]
rightSet = nub . (map snd)

converse :: Rel a b -> Rel b a
converse r = [(b,a) | (a,b) <- r]

con x = converse x

untrans :: Rel a (b,c) -> Rel (a,b) c
untrans r = [((a,b),c)  | (a, (b,c)) <- r]

trans :: Rel (a,b) c -> Rel a (b, c)
trans r = [(a, (b,c))| ((a,b),c)  <- r]

reflectOrd :: (Ord a, BEnum a) => Rel a a
reflectOrd = [(x,y) | x <- enumAll, y <- enumAll, x <= y]

tabulateSearch :: BEnum a => (a -> [b]) -> Rel a b
tabulateSearch f = [(a,b) | a <- enumAll, b <- f a]

searchRel :: Eq a => Rel a b -> (a -> [b])
searchRel r a = [b | (a', b) <- r, a == a']

power :: [a] -> [[a]] -- all subsets. 
power (x:xs) = (power xs) ++ [ x : xs' | xs' <- power xs]  -- x is in or not.

rElem :: BEnum a => Rel a [a]
rElem = [(a,xs) | xs <- power enumAll, a <- xs]

power' :: Eq a => Rel a b -> Rel a [b]
power' r = [ (a, searchRel r a) | a <- leftSet r]


tabulatePartial :: BEnum a => (a -> Maybe b) -> Rel a b
tabulatePartial f = [(a,b) | a <- enumAll, b <- toList (f a)]

reflectInd :: (BoundedMeetSemiLattice (Rel a b)) => (a -> b -> Bool) -> Rel a b -- )BEnum a, BEnum b)
reflectInd f = filter (uncurry f) top

rOrd' :: (Ord a, BEnum a) =>Rel a a
rOrd' = reflectInd (<=)

-- rElem' = reflectInd elem -- going to build the top of [a]? 
-- rOrd' = reflectInd (<=)
instance (Eq a, Eq b, BEnum a, BEnum b) => BoundedMeetSemiLattice (Rel a b) where
    top :: (BEnum a, BEnum b) => Rel a b 
    top = [(x,y) | x <- enumAll, y <- enumAll]
-- all possible tuples

-- because of superclass constraints :(
instance (Eq a, Eq b) => BoundedJoinSemiLattice (Rel a b) where
    bottom :: Rel a b -- no tuples
    bottom = [] 

meet' :: (Eq a, Eq b) => Rel a b -> Rel a b -> Rel a b
meet' xs ys = [x | x <- xs, x `elem` ys] -- intersection
join' :: (Eq a, Eq b) => Rel a b -> Rel a b -> Rel a b
join' p q = nub (p ++ q) -- union


instance (Eq a, Eq b) => Lattice (Rel a b) where
    x /\ y = meet' x y
    x \/ y = join' x y


{-
(/\) :: (Eq a, Eq b) => Rel a b -> Rel a b -> Rel a b
(/\) = meet
(\/) :: (Eq a, Eq b) => Rel a b -> Rel a b -> Rel a b
(\/) = join
-}


rSub :: (Eq a, Eq b) => Rel a b -> Rel a b -> Bool
rSub xs ys = and [x `elem` ys | x <- xs]

x <~ y = rSub x y

rEq :: (Eq a, Eq b) => Rel a b -> Rel a b -> Bool
rEq xs ys = (xs `rSub` ys) && (ys `rSub` xs)

x ~~ y = rEq x y

-- rdiv x y is maximal relation such that (rdiv x y) . y <= x
rdiv :: (BEnum a, BEnum b, Eq a, Eq b, Eq c) => Rel a c -> Rel b c -> Rel a b
rdiv x y = [ (a,b)  | a <- enumAll, b <- enumAll, all (\c -> ((b,c) `elem` y)`implies` ((a,c) `elem` x)) (rightSet y)]

implies p q = (not p) || q 


shrink :: (BEnum a, BEnum b, Eq a, Eq b) => Rel a b -> Rel a a -> Rel a b
shrink r s = r /\ (s `rdiv` (con r))

-- override :: _
size :: forall a. BEnum a => Int
size = (fromEnum (maxBound @a)) - (fromEnum (minBound @a))
instance (Enum a, Enum b, Bounded a, Bounded b) => Enum (a,b) where
    fromEnum (a,b) = (fromEnum a) * (size @b) + (fromEnum b)
    toEnum n = (toEnum na, toEnum nb) where (na, nb) = divMod n (size @b)

-- arrow/category combinators
rfan :: Eq a => Rel a b -> Rel a c -> Rel a (b,c) 
rfan f g = [ (a, (b,c)) | (a,b) <- f, (a',c) <- g, a == a']

rfst :: BEnum (a,b) => Rel (a,b) a 
rfst = tabulate fst -- map (fst . snd) rid

rsnd :: BEnum (a,b) => Rel (a,b) b 
rsnd = tabulate snd --  map (snd . snd) rid

rleft :: (Enum a, Bounded a) => Rel a (Either a b) 
rleft = tabulate Left -- [(a, Left a) | a <- enumAll]-- rmap Left rid

rright :: BEnum b => Rel b (Either a b) 
rright = tabulate Right -- [(a, Right a) | a <- enumAll] -- rmap Right rid

reither :: Eq a => Rel a c -> Rel b c -> Rel (Either a b) c 
reither f g = [(Left a, c) | (a,c) <- f] ++ [(Right b, c) | (b,c) <- g] --  (lmap Left f) ++ (lmap Right g)




--- goofy inefficient definitions
dup :: (Eq a, Eq b, BEnum a, BEnum b) => Rel a (a,a)
dup = rfan rid rid
swap ::(Eq a, Eq b, BEnum (a,b)) => Rel (a,b) (b,a)
swap = rfan rsnd rfst
par :: (Eq a, Eq c, BEnum a, BEnum c) => Rel a b -> Rel c d -> Rel (a,c) (b,d) 
par f g =  rfan (rcompose f rfst) (rcompose g rsnd)

    -- [((a,b), (c,d)) | (a,c) <- f, (b, d) <- g ]

{-
Specialized things for "square" relations
Transition systems, abstract rewrite systes, graphs

reach :: Rel a a -> Rel a a -- transitive closure
symm x = join x (converse x)
 
-}