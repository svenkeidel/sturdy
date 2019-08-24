{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
module Data.Order where

import           Data.Functor.Identity
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as HM
import           Data.HashSet(HashSet)
import qualified Data.HashSet as HS
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import           Data.Text (Text)
import           Data.Identifiable
import           Data.Profunctor

import           Control.Arrow

-- | Reflexive, transitive order
class PreOrd x where
  (⊑) :: x -> x -> Bool
  infix 4 ⊑

  (≈) :: x -> x -> Bool
  x ≈ y = x ⊑ y && y ⊑ x
  infix 4 ≈



-- | Order with all least upper bounds
class PreOrd x => Complete x where
  (⊔) :: x -> x -> x
  infixr 5 ⊔

-- | Order with a least element
class PreOrd x => LowerBounded x where
  bottom :: x

lub :: (Foldable f, Complete x) => f x -> x
lub = foldr1 (⊔)

joined :: (Profunctor c, Complete (c (a1,a2) b)) => c a1 b -> c a2 b -> c (a1,a2) b
joined f1 f2 = (lmap fst f1) ⊔ (lmap snd f2)

lubA :: (ArrowChoice c, Profunctor c, Complete (c (x,[x]) y), LowerBounded (c () y)) => c x y -> c [x] y
lubA f = proc l -> case l of
  [] -> bottom -< ()
  -- (x:xs) -> (f -< x) ⊔ (lubA f -< xs) causes an type error.
  (x:xs) -> joined f (lubA f) -< (x, xs)
{-# DEPRECATED lubA "Use Control.Arrow.Abstract.ArrowJoin.joinList" #-}

-- | Order with all greatest lower bounds
class PreOrd x => CoComplete x where
  (⊓) :: x -> x -> x
  infix 5 ⊓

-- | Order with a greatest element
class PreOrd x => UpperBounded x where
  top :: x

glb :: (Foldable f, CoComplete x) => f x -> x
glb = foldr1 (⊓)

instance (PreOrd e, PreOrd a) => PreOrd (Either e a) where
  Left e1 ⊑ Left e2 = e1 ⊑ e2
  Right a1 ⊑ Right a2 = a1 ⊑ a2
  _ ⊑ _ = False

instance PreOrd a => PreOrd [a] where
  []   ⊑ []   = True
  a:as ⊑ b:bs = a ⊑ b && as ⊑ bs
  _    ⊑ _    = False
  
  []   ≈ []   = True
  a:as ≈ b:bs = a ≈ b && as ≈ bs
  _    ≈ _    = False

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
  (⊑) = (==)
  (≈) = (==)

instance PreOrd Double where
  (⊑) = (==)
  (≈) = (==)

instance PreOrd a => PreOrd (Maybe a) where
  Just x ⊑ Just y = x ⊑ y
  _ ⊑ Just _ = True
  Nothing ⊑ _ = True
  _ ⊑ _ = False
  Just x ≈ Just y = x ≈ y
  Nothing ≈ Nothing = True
  _ ≈ _ = False

instance Complete a => Complete (Maybe a) where
  Just x ⊔ Just y = Just $ x ⊔ y
  Just x ⊔ _ = Just x
  _ ⊔ Just x = Just x
  Nothing ⊔ Nothing = Nothing

instance PreOrd a => LowerBounded (Maybe a) where
  bottom = Nothing

instance PreOrd a => LowerBounded (Set a) where
  bottom = S.empty

instance Identifiable a => PreOrd (HashSet a) where
  xs ⊑ ys = all (\x -> HS.member x ys) xs

instance Identifiable a => Complete (HashSet a) where
  xs ⊔ ys = HS.union xs ys
            
instance (Identifiable a, PreOrd b) => PreOrd (HashMap a b) where
  m1 ⊑ m2 = and (HM.intersectionWith (⊑) m1 m2)

instance (Identifiable a, PreOrd b) => LowerBounded (HashMap a b) where
  bottom = HM.empty

instance (Identifiable a, Complete b) => Complete (HashMap a b) where
  (⊔) = HM.unionWith (⊔)

instance (Ord k, PreOrd v) => LowerBounded (Map k v) where
  bottom = M.empty

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
