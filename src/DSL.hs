{-# LANGUAGE GADTs, TypeOperators, PolyKinds, NoImplicitPrelude, StandaloneDeriving #-}
module DSL where

import Prelude hiding (id, (.))
import Control.Category
import Data.Void
import Data.Functor.Foldable
import GHC.Generics hiding ((:+:))
import Data.Functor.Const
import Data.Coerce
import Control.Arrow ((&&&), (|||))
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
    -- Curry :: Fun (a,b) c -> Fun a (Fun b c)
    -- UnCurry :: Fun a (Fun b c) -> Fun (a,b) c
    -- Apply :: Fun (Fun a b, a) b
    Coerce :: Coercible a b => Fun a b
-- Non GADT representation. Useful for equality at least
data UFun = UId | UComp UFun UFun | UFst | USnd | UFan UFun UFun | UDump | UAbsurd | USplit UFun UFun | ULft | URgt 
            | UDistL | UDistR | UCoerce deriving (Eq, Show, Ord)


instance Eq (Fun a b) where
    x == y = (droptype x) == (droptype y)

droptype :: Fun a b -> UFun 
droptype Id = UId
droptype (Comp f g) = UComp (droptype f) (droptype g)
droptype Fst = UFst
droptype _ = error "Todo drop" -- and so on


-- deriving instance Eq (Fun a b) -- Hmm Comp is a problem here. Can't determinater that the internal types match.
-- deriving instance (Eq a, Eq b) => Eq (Fun a b)
deriving instance (Show (Fun a b))
    --Cata :: Fun (f a) a -> Fun (Fix f) a
    -- GenTo :: Generic a => Fun a (Rep a p) -- Kind problems
    -- GenFrom :: Generic a => Fun (Rep a p) a
    -- Lit :: (a -> b) -> Fun a b

type Rule a b = Fun a b -> Maybe (Fun a b)
idl_absorb :: Rule a b 
idl_absorb (Comp Id f) = Just f
idl_unabsorb :: Rule a b 
idl_unabsorb f = Just (Comp Id f)
idr_absorb :: Rule a b 
idr_absorb (Comp f Id) = Just f




dump_absorb :: Rule a ()
dump_absorb (Comp Dump f) = Just Dump

undump :: Fun a b -> Rule a ()
undump f Dump = Just $ Comp Dump f



dump_reflect :: Rule () ()
dump_reflect Dump = Just Id



fan_absorb :: Rule (a,b) (a,b) --- reflexion 
fan_absorb (Fan Fst Snd) = Just Id 
fan_fst :: Rule a b
fan_fst (Comp Fst (Fan f g)) = Just f 
fan_snd :: Rule a b
fan_snd (Comp Snd (Fan f g)) = Just g

-- the reverses also since they are in fact equalites
fan_fst' :: Fun a c -> Rule a b
fan_fst' g f = Just (Comp Fst (Fan f g))


{-
par_id t@(Fan (Comp Id Fst) (Comp Id Snd)) = do
                                             first idl_absorb
                                             second idl_absorb
                                             fan_absorb
-}
{-
Rule -> Rule
-}

{-

rule is a category btw

rid :: Rule a a
rid = Just

rcompose :: Rule -> Rule -> Rule


-- really when we open up the composition, if the types don't match it should fail

cleft :: (forall a. Rule a b) -> Rule c b
cleft r (Comp f g) = do
                     f' <- r f
                     return (Comp f' g)

cright :: (forall b. Rule a b) -> Rule a c
cright r (Comp f g) = do
                    g' <- r g
                    return (Comp f g')

fleft :: 
fleft r (Fan f g) = 

    -}

{-
Alternative to Rule as axioms

data Thm = And Thm Thm | Or Thm Thm | Not Thm | Eq (Fun a b) (Fun a b) 
data Eq a b = Eq (Fun a b) (Fun a b) 
refl x = Eq x x
symm (Eq a b) = (Eq b a)
trans (Eq a b) (Eq b' c) | b == b' = Eq a c -- syntacic equality

These are reallt axioms schemes though. A little unsatisfying. I want to express universal props internal to the logic.
fan_prop f g = Eq ()

Universal constructions are all instances of stuff (kan ends limits who knows). Could try and take the fanciest one and derive from there?

-}
interp :: Fun a b -> (a -> b) -- or (Cartesian, yada ayda ) => k a b
interp Id = id
interp Fst = fst
interp (Comp f g) = (interp f) . (interp g) 
interp Snd = snd
interp (Fan f g) = (interp f) &&& (interp g)
interp Lft = Left
interp Rgt = Right
interp (Split f g) = (interp f) ||| (interp g)
interp Coerce = coerce
-- interp (Curry f) = curry (interp f) -- require a typeclass?
-- interp (UnCurry f) (a,b) = interp (interp f) a
-- interp Apply = \(f,x) -> (interp f) x


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
-- DistL . (Split Snd Snd) .
ite :: Fun (Bool', (a, a)) a  
ite = (Split Fst Snd) . h3 . h2 . h1 . first (first Coerce) . assocl where
    h1 :: Fun ((() :+: (),a), a)  (((),a) :+: ((),a),a )
    h1 =  (first DistL)
    h2 :: Fun (((),a) :+: ((),a),a ) (a :+: a, a)
    h2 = first $ Split (Lft . Snd) (Rgt . Snd)
    h3 :: Fun  (a :+: a, a) ((a,a) :+: (a,a))
    h3 = DistL





-- DistL . (Split Snd Snd) .
-- ite :: Fun (Bool', (a, a)) a  
--ite = (Split Fst Snd) . DistL . (Par DistL Id) . assocl where




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

