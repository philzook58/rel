{-# LANGUAGE GADTs, RankNTypes, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
module UniRel where


import Data.IntMap.Strict as M
{-
data UniTree a where
    Tup :: UniTree a -> UniTree b -> UniTree (a,b)
    Var :: Int -> UniTree a -- or ghc.unique
    --Bottom :: UniTree a -- bottom is a conflict. ununifiable. Alternative is to use Maybe UniTree
    --Top :: UniTree a -- top is the same thing as a fresh variable.
    --Lef :: UniTree a -> UniTree (Either a b)
    --Rig :: UniTree b -> UniTree (Either a b)
    --Unit :: UniTree ()
    --Fix :: UniTree f (Fix f) -> UniTree (Fix f)
Non Gadt Form -- Generics with Variables.

data E v a b = Lef a | Rig b | EVar v
data T v a b = Tup a b | EVar v
data U v = Unit | UVar v

or intersperse Either Int K


-}

-- Not doing the gadt may be prefereable. 
data UniTree' v = Var v | Tup (UniTree' v) (UniTree' v)
                  | Unit | Lef (UniTree' v) | Rig (UniTree' v) 
                 deriving (Functor, Traversable, Foldable, Show, Eq, Ord)
type UniTree = UniTree' Int

{-
unififcation-fd
Is using this premature optimization


data UniTreeF a = Tup a a | Unit | Lef a | Rig a deriving (Functor, Foldable, Traversable, Applicative)
type UniTree = UTerm UniTreeF IntVar

instance Unifiable UniTreeF where
    zipMatch Unit Unit = 


-}

-- newtype UniTreeE = UniTreeE (forall a. UniTree a) 
-- type Sub = M.IntMap UniTreeE -- (forall a. UniTree a) -- ugh
type Sub = M.IntMap UniTree

-- walk?
-- this is almost a fmap
applySub :: Sub -> UniTree -> UniTree
applySub s (Tup x y) = Tup (applySub s x) (applySub s y)
applySub s (Lef x) = Lef (applySub s x)
applySub s (Rig x) = Rig (applySub s x)
applySub s  Unit     = Unit
applySub s (Var i)   = case M.lookup i s of
                          Nothing -> Var i
                          Just t -> applySub s t
-- annoying that we have n^2 entries
-- could make smaller if we use more underscores. But unsafe.
-- if we used gadt, many of these cases are impossible.
unify :: UniTree -> UniTree -> Maybe Sub 
unify Unit      Unit      = Just M.empty
unify Unit      (Tup _ _) = Nothing
unify Unit      (Lef _)   = Nothing
unify Unit      (Rig _)   = Nothing

unify (Tup x y) (Tup a b) = do
                            s1 <- unify x a
                            s2 <- unify (applySub s1 y) (applySub s1 b)
                            Just $ M.union s1 s2  
unify (Tup _ _) (Lef _)   = Nothing
unify (Tup _ _) Unit      = Nothing
unify (Tup _ _) (Rig _)   = Nothing

unify (Lef x) (Lef a)     = unify x a
unify (Lef _) (Tup _ _)   = Nothing
unify (Lef _) Unit        = Nothing
unify (Lef _) (Rig _)     = Nothing

unify (Rig x) (Rig a)     = unify x a
unify (Rig _) (Tup _ _)   = Nothing
unify (Rig _) Unit        = Nothing
unify (Rig _) (Lef _)     = Nothing

unify (Var i)   x         = Just $ M.singleton i x
unify x         (Var i)   = Just $ M.singleton i x


{-
type DSub = M.Map UniTree Int
antiunify l@(Lef _) (Rig _) = M.fromList [(l, newVar), (r, newVar)]
antiunify 
-}



incrVar :: Int -> UniTree -> UniTree
incrVar n = fmap (+ n)
{-
incrVar n (Var i) = Var (i + n)
incrVar n (Tup x y) = Tup (incrVar n x) (incrVar n y)
incrVar n Unit = Unit-}

maxVar :: UniTree -> Int
maxVar = maximum
{-
maxVar (Var i) = i
maxVar (Tup x y) = max (maxVar x) (maxVar y)
maxVar Unit = 0
-}
{-
applySub :: Sub -> UniTree a -> UniTree a
applySub s (Tup x y) = Tup (applySub s x) (applySub s y)
applySub s (Var i) = case M.lookup i s of
                          Nothing -> Var i
                          Just j -> applySub s (Var j) 

unify :: UniTree a -> UniTree a -> Maybe Sub 
unify (Tup x y) (Tup a b) = do
                            s1 <- unify x a
                            s2 <- unify (applySub s1 y) (applySub s1 b)
                            Just $ M.union s1 s2  
unify (Var i)   x         = Just $ M.singleton i x
unify x         (Var i)   = Just $ M.singleton i x
unify _         _         = Nothing

incrVar :: Int -> UniTree a -> UniTree a
incrVar n (Var i) = Var (i + n)
incrVar n (Tup x y) = Tup (incrVar n x) (incrVar n y)

maxVar :: UniTree a -> Int
maxVar (Var i) = i
maxVar (Tup x y) = max (maxVar x) (maxVar y)
-}
ex1 = unify (Tup (Var 1) (Var 2)) (Var 3)

-- type Fresh a = State Int a
{-
top = do 
      i <- getState
      modifyState (+ 1)
      Var i 
      -}
top :: UniTree -> UniTree
top x = Var $ 1 + (maxVar x)
bottom :: UniLat
bottom = Nothing

type UniLat = Maybe UniTree

meet :: UniLat -> UniLat -> UniLat
meet x y = do
           x' <- x
           y' <- y
           s <- unify x' y' 
           return $ applySub s x'
-- join = antiunify/ generalize

type UniRel = Maybe (UniTree, UniTree)

