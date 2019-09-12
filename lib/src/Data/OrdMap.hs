module Data.OrdMap
( OrdMap
, empty
, insert
, compare
, Ordering (..)
)
where

import           Prelude hiding (Ordering,compare)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Identifiable
import           Data.Order
import qualified Data.Abstract.Boolean as A

-- | Datatype that describes a subset of a heterogenous order with
-- fast access to the lower and upper sets of elements.
data OrdMap n1 n2 = OrdMap (HashMap (n1,n2) Ordering)

data Ordering = LessThanEquals    | Equivalent   | GreaterThanEquals
              | NotLessThanEquals | Incomparable | NotGreaterThanEquals
              | StrictLessThan    | StrictGreaterThan
  deriving (Eq)

instance PreOrd Ordering where
  LessThanEquals       ⊑ Equivalent        = True
  GreaterThanEquals    ⊑ Equivalent        = True

  NotLessThanEquals    ⊑ Incomparable      = True
  NotGreaterThanEquals ⊑ Incomparable      = True

  LessThanEquals       ⊑ StrictLessThan    = True
  NotGreaterThanEquals ⊑ StrictLessThan    = True

  GreaterThanEquals    ⊑ StrictGreaterThan = True
  NotLessThanEquals    ⊑ StrictGreaterThan = True

  x                    ⊑ y                 = x == y

instance Complete Ordering where
  LessThanEquals       ⊔ GreaterThanEquals    = Equivalent
  GreaterThanEquals    ⊔ LessThanEquals       = Equivalent

  NotLessThanEquals    ⊔ NotGreaterThanEquals = Incomparable
  NotGreaterThanEquals ⊔ NotLessThanEquals    = Incomparable

  LessThanEquals       ⊔ NotGreaterThanEquals = StrictLessThan
  NotGreaterThanEquals ⊔ LessThanEquals       = StrictLessThan

  GreaterThanEquals    ⊔ NotLessThanEquals    = StrictGreaterThan
  NotLessThanEquals    ⊔ GreaterThanEquals    = StrictGreaterThan

  x ⊔ y | x ⊑ y     = y
        | y ⊑ x     = x
        | otherwise = error "The ordering is inconcistent"

empty :: OrdMap n1 n2
empty = OrdMap M.empty

insert :: (Identifiable n1, Identifiable n2) => n1 -> n2 -> Ordering -> OrdMap n1 n2 -> OrdMap n1 n2
insert n1 n2 ord (OrdMap m) = OrdMap $ M.insertWith (⊔) (n1,n2) ord m

compare :: (Identifiable n1,Identifiable n2) => n1 -> n2 -> Ordering -> OrdMap n1 n2 -> A.Bool
compare n1 n2 o1 (OrdMap m) = case M.lookup (n1,n2) m of
  Just o2
    | o1 ⊑ o2   -> A.True
    | o2 ⊑ o1   -> A.Top
    | otherwise -> A.False
  Nothing       -> A.Top

instance (Identifiable n1, Identifiable n2) => Semigroup (OrdMap n1 n2) where
  (OrdMap m1) <> (OrdMap m2) = OrdMap (M.unionWith (⊔) m1 m2)

instance (Identifiable n1, Identifiable n2) => Monoid (OrdMap n1 n2) where
  mempty = empty
  mappend = (<>)
