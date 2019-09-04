{-# LANGUAGE TypeOperators, GADTs, DataKinds, RankNTypes, PolyKinds, ScopedTypeVariables,
  PatternSynonyms #-}
module ProRel where
import Data.Profunctor.Composition (Procompose (..), Rift (..))
import Data.Profunctor ((:->))
import Data.Profunctor.Ran
import Data.Type.Equality 
import Data.Void
{-
-- Identical to ProCompose
data RCompose k k' a c where
    RCompose :: k b c -> k' a b -> RCompose k k' a c 
    -}

data Nat = S Nat | Z deriving (Eq, Show)

data Plus a b where
    PZ :: Plus '( 'Z, a) a
    PS :: Plus '( a,b) c -> Plus '( 'S a, b) c 

-- Inequality relation
data LTE a b where
    LTERefl :: LTE n n
    LTESucc :: LTE n m -> LTE n ('S m)

data Mul a b where
    MZ :: Mul '( 'Z, n) 'Z
    MS :: Mul '( n, m) p' -> Plus '( m, p') p -> Mul '( 'S n, m) p

{-
Subtraction as a galois connection

Division
-}

data And a b where
    AndTT :: And '( 'True, 'True) 'True
    AndFU :: And '( 'False, a) 'False
    AndUF :: And '( a, 'False) 'False
data Not a b where
    NotTF :: Not 'True 'False
    NotFT :: Not 'False 'True

data Fan k k' a b where
    Fan :: k a b -> k' a c -> Fan k k' a '(b,c)

type k &&& k' = Fan k k'

data Fst a b where
    Prj1 :: Fst '(a, b) a
    
data Snd a b where
    Prj2 :: Snd '(a, b) b

type Par f g = Fan (f <<< Fst) (g <<< Snd)
data Par' f g a b where
    Par' :: f a b -> g c d -> Par' f g '(a, c) '( b, d)

type Dup  = Fan Id Id
type Swap = Fan Snd Fst
-- Does fan exist in the profunctor package? This one is profunctorial
-- I suppose it works with any bifunctor?


data Split k k' a b where
    CaseLeft :: k a c -> Split k k' ('Left a) c
    CaseRight :: k' b c -> Split k k' ('Right b) c
type k ||| k' = Split k k'

data Inj1 a b where
    Inj1 :: Inj1 a ('Left a)
data Inj2 a b where
    Inj2 :: Inj2 a ('Right a)

-- identity function is the same as Equality
data Id a b where
    IdRefl :: Id a a

type Id' a b = (a :~: b)
newtype Top a b = Top ()
top = Top ()
newtype Bottom a b = Bottom Void

-- monomorphic identy
data IdBool (a :: Bool) (b :: Bool) where
    ReflTrue :: IdBool 'True 'True
    ReflFalse :: IdBool 'False 'False

-- monomorphic top
-- monomorphic boottom

data Trans k a b where
    Trans :: k '(a,b) c -> Trans k a '(b,c)


data RCompose k k' a b where
    RCompose :: k b c -> k' a b -> RCompose k k' a c

{-
-- Procompose is not polykinded?
type RCompose = Procompose
pattern RCompose = Procompose
-}
type k <<< k' = RCompose k k' 
type k >>> k' = RCompose k' k

newtype RSub p q = RSub (p :-> q)
type a ::-> b = RSub a b

data RConverse k a b where -- Shorten to RCon?
    RConverse :: k a b -> RConverse k b a
type RCon = RConverse


data REq k k' = REq {to' :: k :-> k', from' :: k' :-> k }

type IndEq k k' = forall k''. (k'' ::-> k)  <-> (k'' ::-> k')
{-
indeq :: forall k k'. IndEq k k' -> REq k k'
indeq (Iso to from) = REq to'' from'' where
    fred :: RSub k k'
    fred = to (RSub (\k -> k))
    larry :: RSub k' k
    larry = from (RSub id)
    strip :: RSub k k' -> k :-> k'
    strip (RSub t) = t
    to'' :: k :-> k'
    to'' = strip fred
    from'' :: k' :-> k
    from'' = case larry of RSub f -> f
    -}
-- http://stedolan.net/research/semirings.pdf

-- Check out "term rewriting and all that"
-- This is also the reflection without remorse data type
-- this is also freecat
data Star k a b where
    Done :: Star k a a
    Roll :: k b c -> Star k a b -> Star k a c

data KPlus k a b where
    PDone :: k a b -> KPlus k a b
    PRoll :: k b c -> KPlus k a b -> KPlus k a c

type SymClos k a b = RJoin k (RCon k) a b
type RefClos k a b = RJoin k Id a b
{- n-fold composition -}

-- similar to Fin.
-- This is also the Vec n is to list and this is to reflection without remorse
data NFold n k a b where
    One :: k a b -> NFold ('S n) k a b
    More :: k b c -> NFold n k a b -> NFold ('S n) k a b

-- newtype SingArr a b = SingArr (Sing a -> Sing b)
-- type a --> b = SingArr a b

data MapMaybe k a b where
    MapJust :: k a b -> MapMaybe k ('Just a) ('Just b)
    MapNothing :: MapMaybe k 'Nothing 'Nothing

data Cata map k a b where
    Cata :: k fa a -> map (Cata map k) x fa  -> Cata map k ('Fix x) a
data MapList k a b where
    MapCons :: k a b -> MapList k as bs -> MapList k (a ': as) (b ': bs)
    MapNil :: MapList k '[] '[]

data Fix f = Fix (f (Fix f))

-- this is pretty arcane


--     Cata :: (map k) fa fb -> Cata map (map k) a b -> Cata map k a b
data Test a b where
    TestN :: Test 'Nothing 'Z
    TestJ :: Test ('Just n) ('S n)
-- get length of list
-- makes naturals Fix Maybe
ex2 :: Cata MapMaybe Test ('Fix ('Just ('Fix 'Nothing))) ('S 'Z)
ex2 = Cata TestJ (MapJust (Cata TestN MapNothing))
{- 

cata :: (f a -> a) -> (Fix f) -> a
cata f = f (fmap (cata f)) x -}
{-
data HCompose a b where
    HCompose :: HCompose '( k, k' ) ('RCompose k k') -- do I want 'RCpomose or RCompose?
data HApply a b where
    HApply :: k a b -> HApply '( k , a ) b
-- but relations don't have currying?
data UnCurry a b where
    UnCurry :: k a k' -> k' b c -> UnCurry k '(a, b) c
data Curry a b where
    Curry :: k (a,b) c -> k' a k -> Curry k a k'

ex1 :: HApply '( Not , 'True) 'False
ex1 = HApply NotTF
-}
{-

Other operators:
Shrink
Override
Domain Restrict
shunting

Relation properties - iso, etc.
Power


-}

    {-
type IndEq k k' = forall k''. (k <<< k'') :-> (k' <<< k'')
type IndEq' k k' = forall k''. (k'' <<< k) :-> (k'' <<< k')

indeq :: IndEq k k' -> REq k k'
indeq f = REq to from where
    to k = f (RCompose k IdRefl)
    from k' = 
        -}

-- type ProConverse k = CoYoneda (RConverse k)
-- returnConverse p = CoYoneda id id p



newtype Terminal a (b :: ()) = Terminal (Top a b)
newtype Initial (a :: Void) b = Initial (Bottom a b)



-- similar to bifunctor product :*:. 
newtype RMeet k k' a b = RMeet (k a b, k' a b)
type k /\ k' = RMeet k k'  



{-
-- other helper functions
meet_comm
meet_assoc'
join_assoc
join_comm
join_assoc'
-}
newtype RJoin k k' a b = RJoin (Either (k a b) (k' a b))

type k \/ k' = RJoin k k'  

-- Ran / Rift from profunctor. They are both Right Kan? Lift vs extension?

-- it's something like this
-- newtype RDiv g j a b = RDiv { runRDiv :: forall x. g x a -> j x b }
-- type RDiv = Ran
newtype RDiv p q a b = RDiv {runRDiv :: forall x. p x a -> q x b}
-- forall x. RSub (ProCompose x j) g <-> RSub x (RDiv g j)
-- 
{-
-- newtype Galois f g = { forall r p. (RSub (f r) p) <-> RSub r (g p) }
Also consider unit counit form
http://hackage.haskell.org/package/profunctors-5.4/docs/Data-Profunctor-Adjunction.html

data Galois f g = unit :: q  :-> g f p, counit g (f q) :-> p


yoneda - the split representation of relation?


-}
-- 
--
-- newtype RSub' k k' = RSub' { forall x a b. RCompose k x a b ->RCompose k' x a b}

-- COnverse (RCompose k k') <-> Converse k' . Converse k 
--newtype LDiv g j a b = LDiv { runLDiv :: forall x. g a x -> j b x } 
{-

-}

{-
    -- http://hackage.haskell.org/package/profunctors-5.4/docs/Data-Profunctor-Ran.html
    -- universals are hidden in Divisions

type g // j = RDiv g j
-- ? Ddid i do this right? I was just guessin. Maybe I need to reverse more stuff.
type g \\ j = LDiv g j -- parse error on single slash

-- exists gj. forall k. (ProCompose k j :-> g) <-> k :-> gj
data RDiv g j where -- direct universal property
    RDiv :: forall k. (ProCompose k j :-> g) <-> k :-> gj

-}

-- It would be nice to be able to annotate kinds
-- type RShrink :: (r :: a -> b -> *) -> (p :: a -> a -> *) -> (a -> b -> *) 
-- = r /\ (RDiv p (RCon r))
type RShrink r p a b = r /\ (RDiv p (RCon r))
{-
http://hackage.haskell.org/package/profunctors-5.4/docs/Data-Profunctor-Composition.html#v:mu
interesting one
mu :: Category p => Procompose p p :-> p
procomposed :: Category p => Procompose p p a b -> p a b

idl uses (->) as identity. Interesting

-}

data Cons a b where
    Cons :: Cons '(x, xs)  (x ': xs)
 
data Nil a b where
    Nil :: Nil a '[]

data Append a b where
    AppendNil :: Append '( '[], xs) xs
    AppendCons :: Append '( xs, ys) zs -> Append '( x ': xs, ys) ( x ': zs ) 


prop_ridleft ::  (k <<< Id) :-> k
prop_ridleft (RCompose k IdRefl) = k

prop_ridright ::  (Id <<< k) :-> k
prop_ridright (RCompose IdRefl k) = k

prop_meet :: p /\ q :-> p
prop_meet (RMeet (p, q)) = p

prop_join :: p :-> p \/ q
prop_join p = RJoin (Left p)

meet_assoc :: RMeet k (RMeet k' k'') a b -> RMeet (RMeet k k') k'' a b
meet_assoc (RMeet (k, (RMeet (k',k'')))) = RMeet (RMeet (k,k'), k'')


prop_top :: k :-> Top
prop_top _ = top

prop_bottom :: Bottom :-> k
prop_bottom (Bottom x) = absurd x

bottom_compose :: REq (k <<< Bottom) Bottom
bottom_compose = REq (\(RCompose k (Bottom b)) -> absurd b) prop_bottom

data Iso a b = Iso {to :: a -> b, from :: b -> a}
type a <-> b = Iso a b

meet_universal :: (p ::-> RMeet k k') <-> (p ::-> k, p ::-> k')
meet_universal = Iso to from where
    to (RSub f) = (RSub $ \p -> case f p of RMeet (k,k') -> k  , RSub $ \p -> case f p of RMeet (k,k') -> k')
    from (RSub f,RSub g) = RSub $ \p -> RMeet (f p, g p) 

prop_con :: RCon (RCon k) :-> k
prop_con (RConverse (RConverse k)) = k


-- singletonized functions are acceptable as relations also.
{-
type SingleFun a b = (Sing a) -> (Sing b)
-- data RFun a b a' b' where
--     RFun :: (Sing a a' -> Sing b b') -> RFun a b a' b' 
-- newtype RFun a b = RFun
data RId a b where
    RId :: RId a a

data RFst a b where
    RFst :: RFst '(a,b) a  -- should these actually be backticked?

data RSnd a b where
    RSnd :: RSnd '(a,b) b 

-}


-- "doubletons"

{-




prop_meet_assoc
prop_meet_comm

prop_join_univ :: R1 -> R1 -> R1 -> Bool
prop_join_univ x y z = ((x \/ y) <~ z) == ((x <~ z) && (y <~ z))


prop_bottom' :: R1 -> Bool
prop_bottom' x = (x <<< bottom) ~~ (bottom :: R1)

prop_trans_iso :: Rel (Bool, Ordering) Word8 -> Bool
prop_trans_iso x = untrans (trans x) == x

prop_rdiv :: Rel Bool Ordering -> Rel Word8 Ordering -> Bool
prop_rdiv g j = (j <<< (rdiv g j)) <~ g


prop_rdiv' :: Rel Bool Word8 -> Rel Bool Ordering -> Rel Word8 Ordering -> Bool
prop_rdiv' x g j = (x <~ (rdiv g j)) == ((j <<< x) <~ g) 


-}