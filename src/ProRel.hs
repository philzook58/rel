{-# LANGUAGE TypeOperators,  GADTs, DataKinds, RankNTypes #-}
module ProRel where
import Data.Profunctor.Composition (Procompose (..), Rift (..))
import Data.Profunctor ((:->))

{-
-- Identical to ProCompose
data RCompose k k' a c where
    RCompose :: k b c -> k' a b -> RCompose k k' a c 
    -}

type RCompose = Procompose
type k <<< k' = RCompose k k' 
type k >>> k' = RCompose k' k

type RSub p q = p :-> q

data RConverse k a b where -- Shorten to RCon?
    RConverse :: k a b -> RConverse k b a
type RCon = RConverse


-- type ProConverse k = CoYoneda (RConverse k)
-- returnConverse p = CoYoneda id id p





-- similar to bifunctor product :*:. 
newtype RMeet k k' a b = RMeet (k a b, k' a b)
type k /\ k' = RMeet k k'  

meet_assoc :: RMeet k (RMeet k' k'') a b -> RMeet (RMeet k k') k'' a b
meet_assoc (RMeet (k, (RMeet (k',k'')))) = RMeet (RMeet (k,k'), k'')

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

{-
    -- http://hackage.haskell.org/package/profunctors-5.4/docs/Data-Profunctor-Ran.html
    -- universals are hidden in Divisions
newtype RDiv g j a b = RDiv { runRDiv :: forall x. g x a -> j x b }
type g // j = RDiv g j
newtype LDiv g j a b = LDiv { runLDiv :: forall x. g a x -> j b x } -- ? Ddid i do this right? I was just guessin. Maybe I need to reverse more stuff.
type g \\ j = LDiv g j -- parse error on single slash

-- exists gj. forall k. (ProCompose k j :-> g) <-> k :-> gj
data RDiv g j where -- direct universal property
    RDiv :: forall k. (ProCompose k j :-> g) <-> k :-> gj

-}



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