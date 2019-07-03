module MapRel where
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Arrow (second)
import Data.Maybe (mapMaybe)
import ListRel as L

-- Map is a better data structure than lists for relations.
type MapRel a b =  (M.Map a (S.Set b)) 

-- Using AscLists in intermediate structures will help performance.

fromListRel :: (Ord a, Ord b) => Rel a b -> MapRel a b
fromListRel rel = M.fromListWith S.union (map (second S.singleton) rel) -- One way of doing it. Should be a better way

toListRel :: (Ord a, Ord b) => MapRel a b -> Rel a b
toListRel rel = [ (a,b)  | (a, s) <- M.toList rel, b <- S.toList s]

rcompose :: (Ord a, Ord b, Ord c) => MapRel a b -> MapRel b c -> MapRel a c -- can result in maps to empty set. maybe want to trim those to maintain invariant.
rcompose r1 r2 =  (M.map S.unions) (partialcompose r1 r2)  -- M.map (\sb -> S.unions $ mapMaybe (flip M.lookup r2) $ S.toList sb) r1

partialcompose :: (Ord a, Ord b, Ord c) => MapRel a b -> MapRel b c -> M.Map a [S.Set c] 
partialcompose r1 r2 = M.map (\sb -> mapMaybe (flip M.lookup r2) $ S.toList sb) r1

-- until better versions can be made.
-- converse is actually reasonable to implement via ListRel


rid :: (Enum a, Bounded a, Ord a) => MapRel a a
rid = fromListRel L.rid

converse :: (Ord a, Ord b) => MapRel a b -> MapRel b a
converse =  fromListRel . L.converse . toListRel

-- there should be a more efficient way. Given that the tuple is ordered on `a` first.
-- I should use the AscList functions?
trans :: (Ord a, Ord b, Ord c) =>  MapRel (a,b) c -> MapRel a (b, c)
trans =  fromListRel . L.trans . toListRel


meet :: (Ord a, Ord b) => MapRel a b -> MapRel a b -> MapRel a b
meet = M.intersectionWith S.intersection

join :: (Ord a, Ord b) => MapRel a b -> MapRel a b -> MapRel a b
join = M.unionWith S.union

rSub :: (Ord a, Ord b) => MapRel a b -> MapRel a b -> Bool 
rSub x y = M.isSubmapOfBy S.isSubsetOf x y