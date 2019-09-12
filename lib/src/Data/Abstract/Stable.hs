module Data.Abstract.Stable where

import Data.Order
import Data.Hashable

-- | Datatype that signals if the ascending chain stabilized.
data Stable = Stable | Unstable
  deriving (Eq,Show)

instance Semigroup Stable where
  (<>) = (⊔)
  {-# INLINE (<>) #-}

instance Monoid Stable where
  mempty = Stable
  mappend = (<>)
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}

instance PreOrd Stable where
  Stable ⊑ Stable = True
  Stable ⊑ Unstable = True
  Unstable ⊑ Unstable = True
  _ ⊑ _ = False
  (≈) = (==)
  {-# INLINE (⊑) #-}
  {-# INLINE (≈) #-}

instance Complete Stable where
  Stable ⊔ a = a
  a ⊔ Stable = a
  Unstable ⊔ Unstable = Unstable
  {-# INLINE (⊔) #-}

instance LowerBounded Stable where
  bottom = Stable
  {-# INLINE bottom #-}

instance Hashable Stable where
  hashWithSalt s Stable = s `hashWithSalt` (1::Int)
  hashWithSalt s Unstable = s `hashWithSalt` (2::Int)
  {-# INLINE hashWithSalt #-}
