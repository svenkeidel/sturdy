{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Abstract.Contour(CallString,Contour,runContour) where

import           Prelude hiding (id,(.),lookup)

import           Control.Arrow
import           Control.Arrow.Alloc
import           Control.Arrow.Environment
import           Control.Arrow.Fail
import           Control.Arrow.Except
import           Control.Arrow.Fix
import           Control.Arrow.Lift
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Transformer.Reader

import           Control.Category

import           Data.Label
import           Data.Order
import           Data.CallString

-- | Records the k-bounded call string. Meant to be used in
-- conjunction with 'Abstract.BoundedEnvironment'.
newtype Contour c a b = Contour (Reader CallString c a b)

-- | Runs a computation that records a call string. The argument 'k'
-- specifies the maximum length of a call string. All larger call
-- strings are truncated to at most 'k' elements.
runContour :: Arrow c => Int -> Contour c a b -> c a b
runContour k (Contour (Reader f)) = (\a -> (empty k,a)) ^>> f

type instance Fix x y (Contour c) = Contour (Fix x y c)
instance (ArrowFix x y c, ArrowApply c, HasLabel x) => ArrowFix x y (Contour c) where
  -- Pushes the label of the last argument on the call string and truncate the call string in case it reached the maximum length
  fix f = Contour $ Reader $ proc (c,x) -> fix (unlift (push (label x) c) . f . lift) -<< x
    where
      unlift :: (HasLabel x, Arrow c) => CallString -> Contour c x y -> c x y
      unlift c (Contour (Reader f')) = proc x -> do
        y <- f' -< (c,x)
        returnA -< y

instance Arrow c => ArrowAlloc (var,val,env) (var,CallString) (Contour c) where
  -- | Return the variable together with the current call string as address.
  alloc = Contour $ Reader $ proc (l,(x,_,_)) -> returnA -< (x,l)

instance ArrowApply c => ArrowApply (Contour c) where
  app = Contour $ (\(Contour f,x) -> (f,x)) ^>> app

instance ArrowReader r c => ArrowReader r (Contour c) where
  ask = lift ask
  local (Contour (Reader f)) = Contour (Reader ((\(c,(r,x)) -> (r,(c,x))) ^>> local f))

deriving instance Arrow c => Category (Contour c)
deriving instance Arrow c => Arrow (Contour c)
deriving instance ArrowLift Contour
deriving instance ArrowChoice c => ArrowChoice (Contour c)
deriving instance ArrowState s c => ArrowState s (Contour c)
deriving instance ArrowEnv x y env c => ArrowEnv x y env (Contour c)
deriving instance ArrowFail e c => ArrowFail e (Contour c)
deriving instance ArrowExcept (CallString,x) y e c => ArrowExcept x y e (Contour c)

deriving instance PreOrd (c (CallString,x) y) => PreOrd (Contour c x y)
deriving instance LowerBounded (c (CallString,x) y) => LowerBounded (Contour c x y)
deriving instance Complete (c (CallString,x) y) => Complete (Contour c x y)
deriving instance CoComplete (c (CallString,x) y) => CoComplete (Contour c x y)
deriving instance UpperBounded (c (CallString,x) y) => UpperBounded (Contour c x y)
