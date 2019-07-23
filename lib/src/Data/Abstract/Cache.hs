{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Abstract.Cache where

import           Prelude hiding (lookup)
   
import           Data.Identifiable
import           Data.Order
import           Data.Abstract.Widening (Stable(..))
import qualified Data.Abstract.Widening as W
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Empty

class IsEmpty (cache a b) => IsCache cache a b where 
  type Widening cache a b :: *
  lookup     :: a -> cache a b -> Maybe (Stable,b)
  initialize :: a -> b -> Stable -> cache a b -> cache a b
  update     :: Widening cache a b -> a -> b -> cache a b -> ((Stable,b),cache a b)

newtype Cache a b = Cache (HashMap a (Stable,b))

instance IsEmpty (Cache a b) where
  empty = Cache M.empty

instance (Identifiable a, LowerBounded b) => IsCache Cache a b where
  type Widening Cache a b = a -> W.Widening b
  initialize a b st (Cache m) = Cache (M.insertWith (\_ old -> old) a (st,b) m)
  lookup a (Cache m) = M.lookup a m
  update widen x y (Cache cache) = case M.lookup x cache of
    Just (_,yOld) -> let yNew = widen x yOld y in (yNew,Cache (M.insert x yNew cache))
    Nothing -> ((Instable,y),Cache (M.insert x (Instable,y) cache))

-- data Monotone a b where
--   Monotone :: s -> Monotone s ()

-- instance IsEmpty s => IsEmpty (Monotone s ()) where
--   empty = Monotone empty

-- instance (IsEmpty s) => IsCache Monotone s () where
--   type Widening Monotone s () = W.Widening s
--   initialize widen s2 (Monotone s1) = let (_,s3) = widen s1 s2 in (s3,Monotone s3)
--   lookup widen s2 (Monotone s1) = let (stable,_) = widen s1 s2 in Just (stable,())
--   update widen s2 () (Monotone s1) = let (stable,s3) = widen s1 s2
--                                      in ((stable,()),Monotone s3)

-- data Product c1 c2 a b where
--   Product :: c1 a1 b1 -> c2 a2 b2 -> Product c1 c2 (a1,a2) (b1,b2)

-- instance (IsEmpty (c1 a1 b1), IsEmpty (c2 a2 b2)) => IsEmpty (Product c1 c2 (a1,a2) (b1,b2)) where
--   empty = Product empty empty

-- instance (IsCache c1 a1 b1, IsCache c2 a2 b2) => IsCache (Product c1 c2) (a1,a2) (b1,b2) where
--   type Widening (Product c1 c2) (a1,a2) (b1,b2) = (Widening c1 a1 b1,Widening c2 a2 b2)
--   initialize (w1,w2) (a1,a2) (Product c1 c2) =
--     let (a1',c1') = initialize w1 a1 c1
--         (a2',c2') = initialize w2 a2 c2
--     in ((a1',a2'),Product c1' c2')
--   lookup (w1,w2) (a1,a2) (Product c1 c2) = (\(s1,b1) (s2,b2) -> (s1 ⊔ s2,(b1,b2))) <$> lookup w1 a1 c1 <*> lookup w2 a2 c2
--   update (w1,w2) (a1,a2) (b1,b2) (Product c1 c2) =
--     let ((s1,b1'),c1') = update w1 a1 b1 c1
--         ((s2,b2'),c2') = update w2 a2 b2 c2
--     in ((s1 ⊔ s2,(b1',b2')),Product c1' c2')

-- data First c1 c2 a b where
--   First :: c1 a1 () -> c2 a2 b2 -> First c1 c2 (a1,a2) b2

-- instance (IsEmpty (c1 a1 ()), IsEmpty (c2 a2 b2)) => IsEmpty (First c1 c2 (a1,a2) b2) where
--   empty = First empty empty

-- instance (IsCache c1 a1 (), IsCache c2 a2 b2) => IsCache (First c1 c2) (a1,a2) b2 where
--   type Widening (First c1 c2) (a1,a2) b2 = (Widening c1 a1 (),Widening c2 a2 b2)
--   lookup (w1,w2) (a1,a2) (First c1 c2) = (\(s1,_) (s2,b2) -> (s1 ⊔ s2,b2)) <$> lookup w1 a1 c1 <*> lookup w2 a2 c2
--   lookupInit (w1,w2) (a1,a2) (First c1 c2) =
--     let ((a1',(s1,_)),c1') = lookupInit w1 a1 c1
--         ((a2',(s2,b2)),c2') = lookupInit w2 a2 c2
--     in (((a1',a2'),(s1 ⊔ s2,b2)),First c1' c2')
--   update (w1,w2) (a1,a2) b2 (First c1 c2) =
--     let ((s1,_),c1') = update w1 a1 () c1
--         ((s2,b2'),c2') = update w2 a2 b2 c2
--     in ((s1 ⊔ s2,b2'),First c1' c2')
