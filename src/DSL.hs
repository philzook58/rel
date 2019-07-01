{-# LANGUAGE GADTs, TypeOperators #-}
module DSL where


type a :+: b = Either a b

data PF a b where
    Id :: PF a a
    Comp :: PF b c -> PF a b -> PF a c
    Fst :: PF (a,b) a
    Snd :: PF (a,b) b
    Fan :: PF a b -> PF a c -> PF a (b,c)
    Lit :: (a -> b) -> PF a b
    Dump :: PF a ()
    Split :: PF a b -> PF c b -> PF (a :+: c) b
    Lft :: PF a (a :+: b)
    Rgt :: PF b (a :+: b)