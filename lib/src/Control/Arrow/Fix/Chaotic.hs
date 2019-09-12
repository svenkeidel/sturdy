{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Arrow.Fix.Chaotic where

import           Prelude hiding (head)

import           Control.Arrow

import           Data.HashSet (HashSet)
import qualified Data.HashSet as H
import           Data.Identifiable
import           Data.Profunctor
import           Data.Order

import           Text.Printf

class (Arrow c, Profunctor c) => ArrowIterate a c where
  -- | Remembers to iterate on an unstable result until it stabilized.
  iterate :: c (a,b) b

class (Arrow c, Profunctor c) => ArrowComponent a c | c -> a where
  setComponent :: c (Component a,y) y
  withComponent :: c x y -> (c (x,y,Component a) y) -> c x y

data Component a = Component { head :: HashSet a, body :: HashSet a } deriving (Eq)

instance Identifiable a => PreOrd (Component a) where
  c1 ⊑ c2 = head c1 ⊑ head c2 && body c1 ⊑ body c2
  {-# INLINE (⊑) #-}

instance Identifiable a => Complete (Component a) where
  c1 ⊔ c2 = c1 <> c2
  {-# INLINE (⊔) #-}

instance Identifiable a => Semigroup (Component a) where
  Component h1 b1 <> Component h2 b2 = Component { head = h1 <> h2, body = b1 <> b2 }
  {-# INLINE (<>) #-}

instance Identifiable a => Monoid (Component a) where
  mempty = Component { head = H.empty, body = H.empty }
  mappend = (<>)
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}

singleton :: Identifiable a => a -> Component a
singleton a = Component { head = H.singleton a, body = H.empty }
{-# INLINE singleton #-}

instance Show a => Show (Component a) where
  show (Component h b) = printf "Component { head = %s, body = %s }" (show (H.toList h)) (show (H.toList b))
