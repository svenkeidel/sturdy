module Data.Abstract.MonotoneVersioned where

import Control.DeepSeq

import Data.Empty
import Data.Order
import Data.Hashable

-- | Wrapper that encapsulated a datatype that grows monotonically, grow
-- monotonically. In particular, the abstraction features a version number,
-- which allows efficient hashcode, equality and ordering checks. Whenever the
-- store grows strictly, the version number is increased. However, for this
-- abstraction to be valid, the data needs be used in a linear fashion. More
-- specifically, each element can only be used once and duplicating an element
-- is **not** allowed.
data Versioned a = Versioned { element :: !a, version :: !Int }

instance IsEmpty a => IsEmpty (Versioned a) where
  empty = Versioned { element = empty, version = 0}

instance PreOrd (Versioned a) where
  s1 ⊑ s2 = version s1 <= version s2

instance Complete (Versioned a) where
  s1 ⊔ s2
    | version s1 <= version s2 = s2
    | otherwise                = s1

instance Hashable (Versioned a) where
  hashWithSalt salt st = salt `hashWithSalt` version st
  hash st = version st

instance Eq (Versioned a) where
  s1 == s2 = version s1 == version s2

instance Show a => Show (Versioned a) where
  -- show a = show (element a) ++ "@" ++ show (version a)
  show a = show (version a)

{- Just for testing
   instance PreOrd a => PreOrd (Versioned a) where
     s1 ⊑ s2 = element s1 ⊑ element s2

   instance Eq a => Eq (Versioned a) where
     s1 == s2 = element s1 == element s2

   instance Complete a => Complete (Versioned a) where
     s1 ⊔ s2 = Versioned { element = element  s1 ⊔ element s2, version = 0 }

   instance Hashable a => Hashable (Versioned a) where
     hashWithSalt salt st = salt `hashWithSalt` element st
     hash st = hash (element st)
-}

instance NFData a => NFData (Versioned a) where
  rnf (Versioned a _) = rnf a
