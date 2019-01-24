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
import           Control.Arrow.Transformer.Reader

import           Control.Category

import           Data.Label
import           Data.Order
import           Data.CallString

-- | Records the k-bounded call string. Meant to be used in
-- conjunction with 'Abstract.BoundedEnvironment'.
newtype ContourT lab c a b = ContourT (ReaderT (CallString lab) c a b)

-- | Runs a computation that records a call string. The argument 'k'
-- specifies the maximum length of a call string. All larger call
-- strings are truncated to at most 'k' elements.
runContourT :: Arrow c => Int -> ContourT lab c a b -> c a b
runContourT k (ContourT (ReaderT f)) = (\a -> (empty k,a)) ^>> f

type instance Fix x y (ContourT lab c) = ContourT lab (Fix x y c)
instance (ArrowFix x y c, ArrowApply c, HasLabel x lab) => ArrowFix x y (ContourT lab c) where
  -- Pushes the label of the last argument on the call string and truncate the call string in case it reached the maximum length
  fix f = ContourT $ ReaderT $ proc (c,x) -> fix (unwrap c . f . wrap) -<< x
    where
      wrap :: Arrow c => c x y -> ContourT lab c x y
      wrap = lift'

      unwrap :: (HasLabel x lab, Arrow c) => CallString lab -> ContourT lab c x y -> c x y
      unwrap c (ContourT (ReaderT f')) = proc x -> do
        y <- f' -< (push (label x) c,x)
        returnA -< y

instance Arrow c => ArrowAlloc (var,val,env) (var,CallString lab) (ContourT lab c) where
  -- | Return the variable together with the current call string as address.
  alloc = ContourT $ ReaderT $ proc (l,(x,_,_)) -> returnA -< (x,l)

instance ArrowApply c => ArrowApply (ContourT lab c) where
  app = ContourT $ (\(ContourT f,x) -> (f,x)) ^>> app

instance ArrowReader r c => ArrowReader r (ContourT lab c) where
  ask = lift' ask
  local (ContourT (ReaderT f)) = ContourT $ ReaderT $ ((\(c,(r,x)) -> (r,(c,x))) ^>> local f)
  
deriving instance Arrow c => Category (ContourT lab c)
deriving instance Arrow c => Arrow (ContourT lab c)
deriving instance ArrowLift (ContourT lab)
deriving instance ArrowChoice c => ArrowChoice (ContourT lab c)
deriving instance ArrowState s c => ArrowState s (ContourT lab c)
deriving instance ArrowEnv x y env c => ArrowEnv x y env (ContourT lab c)
deriving instance ArrowFail e c => ArrowFail e (ContourT lab c)
deriving instance ArrowExcept e c => ArrowExcept e (ContourT lab c)

deriving instance PreOrd (c (CallString lab,x) y) => PreOrd (ContourT lab c x y)
deriving instance LowerBounded (c (CallString lab,x) y) => LowerBounded (ContourT lab c x y)
deriving instance Complete (c (CallString lab,x) y) => Complete (ContourT lab c x y)
deriving instance CoComplete (c (CallString lab,x) y) => CoComplete (ContourT lab c x y)
deriving instance UpperBounded (c (CallString lab,x) y) => UpperBounded (ContourT lab c x y)
