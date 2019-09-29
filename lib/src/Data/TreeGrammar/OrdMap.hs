module Data.TreeGrammar.OrdMap
( OrdMap
, empty
, insertLeq
, insertNotLeq
, leq
, upper
)
where

import           Prelude hiding (Ordering,compare)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.HashSet (HashSet)
import           Data.Identifiable
import qualified Data.Abstract.Boolean as A
import           Data.TreeGrammar.ClosedSet (ClosedSet)
import qualified Data.TreeGrammar.ClosedSet as C

-- | Datatype that describes a subset of a heterogenous order with
-- fast access to the lower and upper sets of elements.
data OrdMap n1 n2 = OrdMap (HashMap n1 (Boundary n2))
  deriving (Show)

data Boundary n = Boundary { comparable :: ClosedSet n, incomparable :: ClosedSet n }
  deriving (Show)

empty :: OrdMap n1 n2
empty = OrdMap M.empty

insertLeq :: (Identifiable n1, Identifiable n2) => n1 -> HashSet n2 -> OrdMap n1 n2 -> OrdMap n1 n2
insertLeq n1 n2 (OrdMap o) = OrdMap $ M.insertWith (const (\b -> b {comparable = C.insertUpper n2 (comparable b)}))
                                                   n1 (Boundary (C.insert n2 C.empty) C.empty) o

insertNotLeq :: (Identifiable n1, Identifiable n2) => n1 -> HashSet n2 -> OrdMap n1 n2 -> OrdMap n1 n2
insertNotLeq n1 n2 (OrdMap o) = OrdMap $ M.insertWith (const (\b -> b {incomparable = C.insertLower n2 (incomparable b)}))
                                                       n1 (Boundary C.empty (C.insert n2 C.empty)) o 

leq :: (Identifiable n1, Identifiable n2) => n1 -> HashSet n2 -> OrdMap n1 n2 -> A.Bool
leq n1 n2 (OrdMap o) = case M.lookup n1 o of
  Just bounds
    | C.memberUpper n2 (comparable bounds)   -> A.True
    | C.memberLower n2 (incomparable bounds) -> A.False
  _ -> A.Top

upper :: Identifiable n1 => n1 -> OrdMap n1 n2 -> [HashSet n2]
upper n1 (OrdMap o) = case M.lookup n1 o of
  Just bounds -> C.elems (comparable bounds)
  Nothing     -> []
