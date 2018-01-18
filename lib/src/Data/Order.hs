{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
module Data.Order where

import           Data.Functor.Identity
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import           Data.Text (Text)

import           Numeric.Limits

import           Control.Arrow
import           Control.Monad.State
import           Control.Monad.Reader

-- | Reflexive, transitive order
class PreOrd x where
  (⊑) :: x -> x -> Bool

  (≈) :: x -> x -> Bool
  x ≈ y = x ⊑ y && y ⊑ x

-- | Order with all least upper bounds
class PreOrd x => Complete x where
  (⊔) :: x -> x -> x

-- | Order with a least element
class PreOrd x => LowerBounded x where
  bottom :: x

lub :: (Foldable f, Complete x, LowerBounded x) => f x -> x
lub = foldr (⊔) bottom

lubA :: (ArrowChoice c, LowerBounded y, Complete (c (x,[x]) y)) => c x y -> c [x] y
lubA f = proc l -> case l of
  [] -> returnA -< bottom
  -- (x:xs) -> (f -< x) ⊔ (lubA f -< xs) causes an type error.
  (x:xs) -> (proc (a,_) -> f -< a) ⊔ (proc (_,b) -> lubA f -< b) -< (x,xs)

-- | Order with all greatest lower bounds
class PreOrd x => CoComplete x where
  (⊓) :: x -> x -> x

-- | Order with a greatest element
class PreOrd x => UpperBounded x where
  top :: x

glb :: (Foldable f, Complete x, UpperBounded x) => f x -> x
glb = foldr (⊔) top


instance PreOrd a => PreOrd [a] where
  []     ⊑ []     = True
  (a:as) ⊑ (b:bs) = a ⊑ b && as ⊑ bs
  _      ⊑ _      = False

instance (Ord a, PreOrd a) => PreOrd (Set a) where
  s1 ⊑ s2 = all (\x -> any (\y -> x ⊑ y) s2) s1

instance (Ord a, PreOrd a) => Complete (Set a) where
  (⊔) = S.union

instance (Ord a, PreOrd a) => CoComplete (Set a) where
  (⊓) = S.intersection

instance PreOrd () where
  () ⊑ () = True

instance Complete () where
  () ⊔ () = ()

instance (PreOrd a,PreOrd b) => PreOrd (a,b) where
  (a1,b1) ⊑ (a2,b2) = a1 ⊑ a2 && b1 ⊑ b2 

instance (Complete a, Complete b) => Complete (a,b) where
  (a1,b1) ⊔ (a2,b2) = (a1 ⊔ a2, b1 ⊔ b2)

instance (CoComplete a, CoComplete b) => CoComplete (a,b) where
  (a1,b1) ⊓ (a2,b2) = (a1 ⊓ a2, b1 ⊓ b2)

instance PreOrd b => PreOrd (a -> b) where
  _ ⊑ _ = error "f ⊑ g  iff  forall x. f x ⊑ g x"

instance Complete b => Complete (a -> b) where
  f ⊔ g = \x -> f x ⊔ g x

instance (Ord k,PreOrd v) => PreOrd (Map k v) where
  c1 ⊑ c2 = M.keysSet c1 `S.isSubsetOf` M.keysSet c2 && all (\k -> (c1 M.! k) ⊑ (c2 M.! k)) (M.keys c1)

instance PreOrd v => PreOrd (IntMap v) where
  c1 ⊑ c2 = IM.keysSet c1 `IS.isSubsetOf` IM.keysSet c2 && all (\k -> (c1 IM.! k) ⊑ (c2 IM.! k)) (IM.keys c1)

instance (Ord k, Complete v) => Complete (Map k v) where
  (⊔) = M.unionWith (⊔)

instance PreOrd Char where
  (⊑) = (==)
  (≈) = (==)

instance PreOrd Text where
  (⊑) = (==)
  (≈) = (==)

instance PreOrd Int where
  (⊑) = (<=)
  (≈) = (==)

instance PreOrd Double where
  (⊑) = (<=)
  (≈) = (==)

instance LowerBounded Int where
  bottom = minBound

instance UpperBounded Int where
  top = maxBound

instance Complete Int where
  (⊔) = max

instance CoComplete Int where
  (⊓) = min

instance LowerBounded Double where
  bottom = minValue

instance UpperBounded Double where
  top = maxValue

instance Complete Double where
  (⊔) = max

instance CoComplete Double where
  (⊓) = min

instance PreOrd (m (a,s)) => PreOrd (StateT s m a) where
  _ ⊑ _ = error "StateT f ⊑ StateT g  iff  forall x. f x ⊑ g x"

instance Complete (m (a,s)) => Complete (StateT s m a) where
  StateT f ⊔ StateT g = StateT $ \s -> f s ⊔ g s

instance PreOrd (m a) => PreOrd (ReaderT e m a) where
  _ ⊑ _ = error "ReaderT f ⊑ ReaderT g  iff  forall x. f x ⊑ g x"

instance Complete (m a) => Complete (ReaderT r m a) where
  ReaderT f ⊔ ReaderT g = ReaderT $ \r -> f r ⊔ g r

instance PreOrd (m b) => PreOrd (Kleisli m a b) where
  _ ⊑ _ = error "Kleisli f ⊑ Kleisli g  iff  forall x. f x ⊑ g x"

instance Complete (m b) => Complete (Kleisli m a b) where
  Kleisli f ⊔ Kleisli g = Kleisli $ f ⊔ g

instance PreOrd a => PreOrd (Identity a) where
  (Identity x) ⊑ (Identity y) = x ⊑ y

instance Complete a => Complete (Identity a) where
  Identity x ⊔ Identity y = Identity $ x ⊔ y
