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

joined :: (Arrow c, Complete (c (a1,a2) b)) => c a1 b -> c a2 b -> c (a1,a2) b
joined f1 f2 = (arr fst >>> f1) ⊔ (arr snd >>> f2)

-- What we actually would like to write is this:
-- joined f1 f2 = proc (x1,x2) -> (f1 -< x1) ⊔ (f2 -< x2)

-- This is what the compiler desugars it to (we simplified it):
-- joined = \ @ t_a8le    @ t_a8ld    @ t_a8m7    @ t_a8m3    @ t_a8m5    @ t_a8lm    @ t_a8li    @ t_a8lk    $dArrow_a8pD    $dArrow_a8pE    $dArrow_a8pF    f1_a8kp    f2_a8kq ->
--       (arr (\(x1, x2) -> ((x2, x1), ())) )
--       >>>
--       (op
--             (arr (\((x2,x1), b) -> (x1, b))) >>> arr fst >>> f1   ::  c ((x2,x1,b)) r
--             (arr (\((x2,x1),b) -> (x2,b)) >>> arr fst >>> f2   ::     c ((x2,x1),b) r

lubA :: (ArrowChoice c, LowerBounded y, Complete (c (x,[x]) y)) => c x y -> c [x] y
lubA f = proc l -> case l of
  [] -> returnA -< bottom
  -- (x:xs) -> (f -< x) ⊔ (lubA f -< xs) causes an type error.
  (x:xs) -> joined f (lubA f) -< (x, xs)

-- | Order with all greatest lower bounds
class PreOrd x => CoComplete x where
  (⊓) :: x -> x -> x

-- | Order with a greatest element
class PreOrd x => UpperBounded x where
  top :: x

glb :: (Foldable f, CoComplete x, UpperBounded x) => f x -> x
glb = foldr (⊓) top

instance (PreOrd e, PreOrd a) => PreOrd (Either e a) where
  Left e1 ⊑ Left e2 = e1 ⊑ e2
  Right a1 ⊑ Right a2 = a1 ⊑ a2
  _ ⊑ _ = False

instance PreOrd a => PreOrd [a] where
  []     ⊑ []     = True
  (a:as) ⊑ (b:bs) = a ⊑ b && as ⊑ bs
  _      ⊑ _      = False

instance PreOrd a => PreOrd (Set a) where
  s1 ⊑ s2 = all (\x -> any (\y -> x ⊑ y) s2) s1

instance (Ord a, PreOrd a) => Complete (Set a) where
  (⊔) = S.union

instance (Ord a, PreOrd a) => CoComplete (Set a) where
  (⊓) = S.intersection

instance PreOrd () where
  () ⊑ () = True

instance LowerBounded () where
  bottom = ()

instance UpperBounded () where
  top = ()

instance Complete () where
  () ⊔ () = ()

instance (PreOrd a,PreOrd b) => PreOrd (a,b) where
  (a1,b1) ⊑ (a2,b2) = a1 ⊑ a2 && b1 ⊑ b2 

instance (LowerBounded a,LowerBounded b) => LowerBounded (a,b) where
  bottom = (bottom,bottom)

instance (UpperBounded a,UpperBounded b) => UpperBounded (a,b) where
  top = (top,top)

instance (Complete a, Complete b) => Complete (a,b) where
  (a1,b1) ⊔ (a2,b2) = (a1 ⊔ a2, b1 ⊔ b2)

instance (CoComplete a, CoComplete b) => CoComplete (a,b) where
  (a1,b1) ⊓ (a2,b2) = (a1 ⊓ a2, b1 ⊓ b2)

instance (PreOrd a,PreOrd b,PreOrd c) => PreOrd (a,b,c) where
  (a1,b1,c1) ⊑ (a2,b2,c2) = a1 ⊑ a2 && b1 ⊑ b2 && c1 ⊑ c2 

instance (LowerBounded a,LowerBounded b,LowerBounded c) => LowerBounded (a,b,c) where
  bottom = (bottom,bottom,bottom)

instance (UpperBounded a,UpperBounded b,UpperBounded c) => UpperBounded (a,b,c) where
  top = (top,top,top)

instance (Complete a, Complete b, Complete c) => Complete (a,b,c) where
  (a1,b1,c1) ⊔ (a2,b2,c2) = (a1 ⊔ a2, b1 ⊔ b2, c1 ⊔ c2)

instance (CoComplete a, CoComplete b, CoComplete c) => CoComplete (a,b,c) where
  (a1,b1,c1) ⊓ (a2,b2,c2) = (a1 ⊓ a2, b1 ⊓ b2, c1 ⊓ c2)

instance PreOrd b => PreOrd (a -> b) where
  _ ⊑ _ = error "f ⊑ g  iff  forall x. f x ⊑ g x"

instance LowerBounded b => LowerBounded (a -> b) where
  bottom = const bottom

instance UpperBounded b => UpperBounded (a -> b) where
  top = const top

instance Complete b => Complete (a -> b) where
  f ⊔ g = \x -> f x ⊔ g x

instance CoComplete b => CoComplete (a -> b) where
  f ⊓ g = \x -> f x ⊓ g x

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

instance PreOrd a => PreOrd (Maybe a) where
  Just x ⊑ Just y = x ⊑ y 
  Nothing ⊑ Nothing = True
  _ ⊑ _ = False
  Just x ≈ Just y = x ≈ y 
  Nothing ≈ Nothing = True
  _ ≈ _ = False

instance PreOrd a => LowerBounded (Set a) where
  bottom = S.empty

instance (Ord k, PreOrd v) => LowerBounded (Map k v) where
  bottom = M.empty

instance PreOrd (m (a,s)) => PreOrd (StateT s m a) where
  _ ⊑ _ = error "StateT f ⊑ StateT g  iff  forall x. f x ⊑ g x"

instance LowerBounded (m (a,s)) => LowerBounded (StateT s m a) where
  bottom = StateT (const bottom)

instance Complete (m (a,s)) => Complete (StateT s m a) where
  StateT f ⊔ StateT g = StateT $ \s -> f s ⊔ g s

instance PreOrd (m a) => PreOrd (ReaderT e m a) where
  _ ⊑ _ = error "ReaderT f ⊑ ReaderT g  iff  forall x. f x ⊑ g x"

instance LowerBounded (m a) => LowerBounded (ReaderT r m a) where
  bottom = ReaderT (const bottom)

instance Complete (m a) => Complete (ReaderT r m a) where
  ReaderT f ⊔ ReaderT g = ReaderT $ \r -> f r ⊔ g r

instance PreOrd (m b) => PreOrd (Kleisli m a b) where
  _ ⊑ _ = error "Kleisli f ⊑ Kleisli g  iff  forall x. f x ⊑ g x"

instance LowerBounded (m b) => LowerBounded (Kleisli m a b) where
  bottom = Kleisli bottom

instance Complete (m b) => Complete (Kleisli m a b) where
  Kleisli f ⊔ Kleisli g = Kleisli $ f ⊔ g

instance PreOrd a => PreOrd (Identity a) where
  (Identity x) ⊑ (Identity y) = x ⊑ y

instance LowerBounded a => LowerBounded (Identity a) where
  bottom = Identity bottom

instance Complete a => Complete (Identity a) where
  Identity x ⊔ Identity y = Identity $ x ⊔ y
  
