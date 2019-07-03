module LogicTRel where
import Control.Monad.Logic
import ListRel as L
import Control.Monad
-- ???????????????
-- LogicT based implementation as alternative to list based implementation?
-- does that actually make any sense? Fair interleave.
type LogicRel a b = Logic (a,b)

fromList :: [a] -> Logic a
fromList = foldr mplus mzero . map return

fromRelList :: L.Rel a b -> LogicRel a b
fromRelList = fromList

toRelList r = runLogic r (:) []



-- should I be allowing for another monad in the fray? LogicT vs Logic.

-- The implementations of these are largely the same as ListRel
-- just desugar the list comprehension and reaplce >>= with >>-

meet :: (Eq a, Eq b) => LogicRel a b -> LogicRel a b -> LogicRel a b 
meet x y = x >>- (\x' -> mfilter (\y' -> x' == y') y)

join :: LogicRel a b -> LogicRel a b -> LogicRel a b
join = interleave

topNat = fromRelList [(x,x) | x <- [0 ..]]

bottom :: LogicRel a b
bottom = mzero

nats :: (Num a, Enum a) => Logic a
nats = fromList [0 ..]

leq :: (Num a, Ord a, Enum a) => LogicRel a a
leq = nats >>- (\x -> mfilter (\y -> x <= y) nats >>= \y -> return (x,y))


converse :: LogicRel a b -> LogicRel b a
converse = fmap swap where swap (x,y) = (y,x)

ridnat :: (Num a, Enum a) => LogicRel a a
ridnat = fromRelList [(x,x) | x <- [0 .. ]]

rcompose ::(Eq b) => LogicRel b c -> LogicRel a b -> LogicRel a c
rcompose f g = f >>- (\(b,c) -> (mfilter (\(_,b') -> b == b') g) >>= (\(a,_) -> return (a,c)))
-- maybe I should still be using some list comprehension syntax extended to monads.


tabulateSearch :: Logic a -> (a -> Logic b) -> LogicRel a b
tabulateSearch as f = as >>- (\a -> f a >>= \b -> return (a,b))

tabulate :: Logic a -> (a -> b) -> LogicRel a b
tabulate as f = fmap (\a -> (a,f a)) as


-- is division even going to be possible?

-- is relational inclusion going to be possible?
-- basically seems like no. unless the relations are ultimately finite.
-- hmm. That is a big wrench in the works.
-- maybe we need to rethink our representation.


-- https://hackage.haskell.org/package/control-monad-omega-0.2/docs/Control-Monad-Omega.html
{-


ridBEnum = foldr mplus mzero L.rid 

topnat = 

import Control.Monad.Logic


nats = fromList [0..]
main = print $ observeMany 10 $ nats >>- \x -> nats >>- \y -> return (x,y)
-}