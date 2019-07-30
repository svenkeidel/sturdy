{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Abstract.Cache where

import           Prelude hiding (lookup)

import           Control.Arrow

import           Data.Empty
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Identifiable
import           Data.Order

import           Data.Abstract.Widening (Stable(..))
import qualified Data.Abstract.Widening as W

class IsEmpty (cache a b) => IsCache cache a b where 
  type Widening cache a b :: *
  -- | Initializes a cache entry with a start value. Initializing the
  -- same entry subsequently does not change the cache ('initialize'
  -- is idempotent).
  initialize :: a -> cache a b -> cache a b

  -- | Overwrites a cache entry.
  insert     :: a -> b -> Stable -> cache a b -> cache a b
  
  -- | Set a cache entry to stable.
  -- adjust     :: ((Stable,b) -> (Stable,b)) -> a -> cache a b -> cache a b
  setStable  :: a -> cache a b -> cache a b

  -- | Updates a cache entry with a widening operator and returns the widened result.
  update     :: Widening cache a b -> a -> b -> cache a b -> ((Stable,b),cache a b)

  -- | Looks up a cache entry.
  lookup     :: a -> cache a b -> Maybe (Stable,b)

newtype Cache a b = Cache (HashMap a (Stable,b))

instance IsEmpty (Cache a b) where
  empty = Cache M.empty

instance (Identifiable a, LowerBounded b) => IsCache Cache a b where
  type Widening Cache a b = W.Widening b
  initialize a (Cache m) = Cache (M.insertWith (\_ old -> old) a (Instable,bottom) m)
  insert a b st (Cache m) = Cache (M.insert a (st,b) m)
  setStable a (Cache m) = Cache (M.adjust (first (\_ -> Stable)) a m)
  update widen x y (Cache cache) = case M.lookup x cache of
    Just (_,yOld) -> let yNew = widen yOld y in (yNew,Cache (M.insert x yNew cache))
    Nothing -> ((Instable,y),Cache (M.insert x (Instable,y) cache))
  lookup a (Cache m) = M.lookup a m

data Monotone s a b where
  Monotone :: Stable -> s -> Monotone s a s

instance IsEmpty s => IsEmpty (Monotone s a s) where
  empty = Monotone Instable empty

instance IsEmpty s => IsCache (Monotone s) a s where
  type Widening (Monotone s) a s = (W.Widening s)
  initialize _ m = m
  insert _ s _ _ = (Monotone Instable s)
  setStable _ m = m
  update widen _ sNew (Monotone _ sOld) =
    let (st,s') = widen sOld sNew
    in ((st,s'), Monotone st s')
  lookup _ (Monotone _ s) = Just (Instable,s)

data (**) c1 c2 a b where
  Product :: c1 a1 b1 -> c2 a2 b2 -> (**) c1 c2 (a1,a2) (b1,b2)

instance (IsEmpty (c1 a1 b1), IsEmpty (c2 a2 b2)) => IsEmpty ((**) c1 c2 (a1,a2) (b1,b2)) where
  empty = Product empty empty

instance (IsCache c1 a1 b1, IsCache c2 a2 b2) => IsCache ((**) c1 c2) (a1,a2) (b1,b2) where
  type Widening ((**) c1 c2) (a1,a2) (b1,b2) = (Widening c1 a1 b1,Widening c2 a2 b2)
  initialize (a1,a2) (Product c1 c2) =
    let c1' = initialize a1 c1
        c2' = initialize a2 c2
    in Product c1' c2'
  insert (a1,a2) (b1,b2) s (Product c1 c2) = Product (insert a1 b1 s c1) (insert a2 b2 s c2)
  setStable (a1,a2) (Product c1 c2) = Product (setStable a1 c1) (setStable a2 c2)
  update (w1,w2) (a1,a2) (b1,b2) (Product c1 c2) =
    let ((s1,b1'),c1') = update w1 a1 b1 c1
        ((s2,b2'),c2') = update w2 a2 b2 c2
    in ((s1 ⊔ s2,(b1',b2')),Product c1' c2')
  lookup (a1,a2) (Product c1 c2) = (\(s1,b1) (s2,b2) -> (s1 ⊔ s2,(b1,b2))) <$> lookup a1 c1 <*> lookup a2 c2

data (&&) c1 c2 a b where
  And :: c1 a b1 -> c2 a b2 -> (&&) c1 c2 a (b1,b2)

instance (IsEmpty (c1 a b1), IsEmpty (c2 a b2)) => IsEmpty ((&&) c1 c2 a (b1,b2)) where
  empty = And empty empty

instance (IsCache c1 a b1, IsCache c2 a b2) => IsCache ((&&) c1 c2) a (b1,b2) where
  type Widening ((&&) c1 c2) a (b1,b2) = (Widening c1 a b1,Widening c2 a b2)
  initialize a (And c1 c2) =
    let c1' = initialize a c1
        c2' = initialize a c2
    in And c1' c2'
  insert a (b1,b2) s (And c1 c2) = And (insert a b1 s c1) (insert a b2 s c2)
  setStable a (And c1 c2) = And (setStable a c1) (setStable a c2)
  update (w1,w2) a (b1,b2) (And c1 c2) =
    let ((s1,b1'),c1') = update w1 a b1 c1
        ((s2,b2'),c2') = update w2 a b2 c2
    in ((s1 ⊔ s2,(b1',b2')),And c1' c2')
  lookup a (And c1 c2) = (\(s1,b1) (s2,b2) -> (s1 ⊔ s2,(b1,b2))) <$> lookup a c1 <*> lookup a c2
