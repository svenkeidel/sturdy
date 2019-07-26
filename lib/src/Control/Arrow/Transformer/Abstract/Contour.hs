{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Abstract.Contour(ContourT,runContourT,contour,CallString) where

import Prelude hiding (id,(.),lookup)

import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Environment
import Control.Arrow.Fail
import Control.Arrow.Except
import Control.Arrow.Fix
import Control.Arrow.Trans
import Control.Arrow.Reader as Reader
import Control.Arrow.State
import Control.Arrow.Order
import Control.Arrow.Transformer.Reader

import Control.Category

import Data.Label
import Data.CallString
import Data.Profunctor
import Data.Profunctor.Unsafe((.#))
import Data.Coerce

-- | Records the k-bounded call string. Meant to be used in
-- conjunction with 'Abstract.BoundedEnvironment'.
newtype ContourT lab c a b = ContourT (ReaderT (CallString lab) c a b)
  deriving (Profunctor,Category,Arrow,ArrowLift,ArrowChoice,
            ArrowConst r, ArrowState s,
            ArrowEnv var val, ArrowClosure var val env,
            ArrowFail e, ArrowExcept e, ArrowLowerBounded, ArrowComplete z)

-- | Runs a computation that records a call string. The argument 'k'
-- specifies the maximum length of a call string. All larger call
-- strings are truncated to at most 'k' elements.
runContourT :: (Arrow c, Profunctor c) => Int -> ContourT lab c a b -> c a b
runContourT k (ContourT (ReaderT f)) = lmap (\a -> (empty k,a)) f
{-# INLINE runContourT #-}

contour :: (Arrow c, Profunctor c) => ContourT lab c () (CallString lab)
contour = ContourT Reader.ask

instance ArrowRun c => ArrowRun (ContourT lab c) where
  type Rep (ContourT lab c) x y = Int -> Rep c x y
  run f i = run (runContourT i f)
  {-# INLINE run #-}

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

instance (ArrowApply c, Profunctor c) => ArrowApply (ContourT lab c) where
  app = ContourT (app .# first coerce)

instance ArrowReader r c => ArrowReader r (ContourT lab c) where
  ask = lift' Reader.ask
  local (ContourT (ReaderT f)) = ContourT $ ReaderT $ ((\(c,(r,x)) -> (r,(c,x))) ^>> Reader.local f)
