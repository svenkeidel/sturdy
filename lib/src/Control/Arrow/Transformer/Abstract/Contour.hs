{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Abstract.Contour(CallString,ContourT,runContourT) where

import           Prelude hiding (id,(.),lookup)

import           Control.Arrow
import           Control.Arrow.Alloc
import           Control.Arrow.Environment
import           Control.Arrow.Fail
import           Control.Arrow.Except
import           Control.Arrow.Fix
import           Control.Arrow.Trans
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Abstract.Join
import           Control.Arrow.Transformer.Reader

import           Control.Category

import           Data.Label
import           Data.Order
import           Data.CallString
import           Data.Profunctor

-- | Records the k-bounded call string. Meant to be used in
-- conjunction with 'Abstract.BoundedEnvironment'.
newtype ContourT lab c a b = ContourT (ReaderT (CallString lab) c a b)
  deriving (Profunctor,Category,Arrow,ArrowLift,ArrowChoice, ArrowState s,
            ArrowEnv x y env, ArrowFail e, ArrowExcept e, ArrowJoin)

-- | Runs a computation that records a call string. The argument 'k'
-- specifies the maximum length of a call string. All larger call
-- strings are truncated to at most 'k' elements.
runContourT :: (Arrow c, Profunctor c) => Int -> ContourT lab c a b -> c a b
runContourT k (ContourT (ReaderT f)) = lmap (\a -> (empty k,a)) f
{-# INLINE runContourT #-}

type instance Fix x y (ContourT lab c) = ContourT lab (Fix x y c)
instance (ArrowFix x y c, ArrowApply c, HasLabel x lab,Profunctor c) => ArrowFix x y (ContourT lab c) where
  -- Pushes the label of the last argument on the call string and truncate the call string in case it reached the maximum length
  fix f = ContourT $ ReaderT $ proc (c,x) -> fix (unwrap c . f . wrap) -<< x
    where
      wrap :: (Arrow c, Profunctor c) => c x y -> ContourT lab c x y
      wrap = lift'

      unwrap :: (HasLabel x lab, Arrow c, Profunctor c) => CallString lab -> ContourT lab c x y -> c x y
      unwrap c (ContourT (ReaderT f')) = proc x -> do
        y <- f' -< (push (label x) c,x)
        returnA -< y
  {-# INLINE fix #-}

instance (Arrow c, Profunctor c) => ArrowAlloc (var,val,env) (var,CallString lab) (ContourT lab c) where
  -- | Return the variable together with the current call string as address.
  alloc = ContourT $ ReaderT $ arr $ \(l,(x,_,_)) -> (x,l)
  {-# INLINE alloc #-}

instance (ArrowApply c, Profunctor c) => ArrowApply (ContourT lab c) where
  app = ContourT $ lmap (\(ContourT f,x) -> (f,x)) app
  {-# INLINE app #-}

instance ArrowReader r c => ArrowReader r (ContourT lab c) where
  ask = lift' ask
  {-# INLINE ask #-}
  local (ContourT (ReaderT f)) = ContourT $ ReaderT $ ((\(c,(r,x)) -> (r,(c,x))) ^>> local f)
  {-# INLINE local #-}

deriving instance PreOrd (c (CallString lab,x) y) => PreOrd (ContourT lab c x y)
deriving instance LowerBounded (c (CallString lab,x) y) => LowerBounded (ContourT lab c x y)
deriving instance Complete (c (CallString lab,x) y) => Complete (ContourT lab c x y)
deriving instance CoComplete (c (CallString lab,x) y) => CoComplete (ContourT lab c x y)
deriving instance UpperBounded (c (CallString lab,x) y) => UpperBounded (ContourT lab c x y)
