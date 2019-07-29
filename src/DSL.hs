{-# LANGUAGE GADTs, TypeOperators, PolyKinds, NoImplicitPrelude #-}
module DSL where

import Prelude hiding (id, (.))
import Control.Category
import Data.Void
import Data.Functor.Foldable
import GHC.Generics hiding ((:+:))
import Data.Functor.Const
import Data.Coerce
type a :+: b = Either a b
-- type Void = V1
-- type Unit = U1
-- basically the initial form of the final DSL of Category, Cartesian, Closed, CoCartesian
-- See Conal Elliott's Compiling to Categories
data Fun a b where
    Id :: Fun a a
    Comp :: Fun b c -> Fun a b -> Fun a c
    Fst :: Fun (a,b) a
    Snd :: Fun (a,b) b
    Fan :: Fun a b -> Fun a c -> Fun a (b,c)
    Dump :: Fun a ()
    Absurd :: Fun Void a 
    Split :: Fun a b -> Fun c b -> Fun (a :+: c) b
    Lft :: Fun a (a :+: b)
    Rgt :: Fun b (a :+: b)
    DistL :: Fun (b :+: c, a) ((b, a) :+: (c, a))
    DistR :: Fun  ((b, a) :+: (c, a)) (b :+: c, a)

    -- Hylo
    -- Ana
    Curry :: Fun (a,b) c -> Fun (Fun a b) c
    UnCurry :: Fun (Fun a b) c -> Fun (a,b) c
    Apply :: Fun (Fun a b, a) b
    Coerce :: Coercible a b => Fun a b

    Cata :: Fun (f a) a -> Fun (Fix f) a
    -- GenTo :: Generic a => Fun a (Rep a p)
    -- GenFrom :: Generic a => Fun (Rep a p) a
    -- Lit :: (a -> b) -> Fun a b
newtype Bool' = Bool' (() :+: ())
newtype NatF a = NatF (() :+: a)
type Nat = Fix NatF

par :: Fun a b -> Fun c d -> Fun (a,c) (b,d)  
par f g = Fan (f . Fst) (g . Snd)

dup :: Fun a (a,a)
dup = Fan Id Id

swap :: Fun (a,b) (b,a)
swap = Fan Snd Fst

assocr :: Fun ((a,b),c) (a,(b,c)) 
assocr = Fan (Fst . Fst) (Fan (Snd . Fst) Snd)

assocl :: Fun (a,(b,c)) ((a,b),c) 
assocl = Fan (Fan Fst (Fst . Snd))  (Snd . Snd) 


first :: Fun a b -> Fun (a,c) (b,c)
first f = par f Id 
second :: Fun a b -> Fun (c,a) (c ,b)
second g = par Id g  
-- dist :: Fun (b :+: c, a) ((b, a) :+: (c, a))
-- dist = Split Id Id :: Fun (a,b) :+: ()
{-
ite :: Fun (Bool', (a, a)) a  
ite = (Split Fst Snd) . DistL . (Par DistL Id) . assocl where
     help :: ((Bool',a), a)  ((a,a) :+: (a,a))
     help = (Par Fst . DistL Id)
 -}  


-- PF is something like the initial encoding of the Arrow typeclass
-- instance Arrow PF
{-
data Fun a b where
    Id :: Fun a a
    Comp :: Fun b c -> Fun a b -> Fun a c
    Fst :: Fun (a :*: b) a
    Snd :: Fun (a :*: b) b
    Fan :: Fun a b -> Fun a c -> Fun a (b :*: c)
    Dump :: Fun a Unit
    Absurd :: Fun Void a 
    Split :: Fun a b -> Fun c b -> Fun (a :+: c) b
    Lft :: Fun a (a :+: b)
    Rgt :: Fun b (a :+: b)
    -- Cata :: PF (f a) a -> PF (Fix f) a
    -- Hylo
    -- Ana
    -- Curry :: Fun (a,b) c -> Fun (Fun a b) c
    -- UnCurry :: Fun (Fun a b) c -> Fun (a,b) c
    -- Apply :: Fun (Fun a b, a)
    -- Coerce :: Coercible a b => Fun a b
    GenTo :: Generic a => Fun (Const a) (Rep a)
    GenFrom :: Generic a => Fun (Rep a) (Const a)
    -- Lit :: (a -> b) -> Fun a b

bool :: Fun (Const Bool) (Unit :+: Unit)
bool = GenTo  
-}
instance Category Fun where
    id = Id
    f . g = Comp f g

