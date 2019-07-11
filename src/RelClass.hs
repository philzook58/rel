module RelClass where
{-

most of them need typeclass constraints
like constrained categroies
Relation ~ Category + Lattice

class Relation k where
    type RelC
    rcompose 
    rid
    meet
    join
    converse
    top 
    bottom
    rsub

req = (rsub x y) && (rsub y x)

class Category, Lattice => Relation
   converse :: k a b -> k b a
   rSub :: 




newtype NatRel a b = NatRel Integer

makeNatRel :: NatRel () ()
makeNatRel x = NatRel x

instance Category NatRel where
    id = NatRel 0
    (NatRel x) . (NatRel y) = NatRel (x + y)

a /\ b = min a b 
a \/ b = max a b
top = NatRel Inifnity
bottom = NatRel 0  
rSub a b = a <= b
rEq a b = a == b
    
   
   -}