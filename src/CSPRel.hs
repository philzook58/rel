{-# LANGUAGE GADTs #-}
module CSPRel where

import ListRel as L
-- A product representation of a relation. Analog of interval arithmetic.
-- Here join is not tight

-- I don't like this. where the list boundaries are hsould be in the type.
data ListTree a where
    Tup :: ListTree a -> ListTree b -> ListTree (a,b)
    Expand :: [a] -> ListTree a -- Set a?

-- maybe CSPRel is too bold a name. ProdRel?
type CSPRel a b = (ListTree a, ListTree b)

expand' :: ListTree a -> [a]
expand' (Tup xs ys) = [(x,y) | x <- expand' xs, y <- expand' ys] 
expand' (Expand xs) = xs

expand = Expand . expand'

join :: ListTree a -> ListTree a -> ListTree a
join (Expand xs) (Expand ys)   = Expand (xs ++ ys) 
join (Tup x y) (Tup a b)       = Tup (join x a) (join y b)
join x@(Tup _ _) z@(Expand zs) = join (expand x) z
join z@(Expand zs) x@(Tup _ _) = join (expand x) z


-- rid = (L.enumAll, L.enumAll)


{-
instance (Enumerable a) -> Enumerable ListTree a where


instance Top a => Top (ListTree a) where
    top 
-}

