module PathRel where

import ListRel
import Algebra.Lattice
-- import Algerba.Lattice.Ordered
type PathRel r a b = [(a,b,r)]
-- import Algebra.PartialOrd

compose' x y = [ ( a,c   , r' /\ r)   | (b,c,r') <- x, (a,b',r) <- y , b == b']

-- rSub' x y = all [   r `leq lookup (a,b) y  | (a,b,r) <- x ] -- a little more complicatedt han this.

