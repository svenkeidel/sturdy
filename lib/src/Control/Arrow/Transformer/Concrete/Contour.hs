{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Concrete.Contour(CallString,ContourT,runContourT) where

import           Prelude hiding (id,(.),lookup)

import           Control.Arrow
import           Control.Arrow.Environment
import           Control.Arrow.Fail
import           Control.Arrow.Except
import           Control.Arrow.Fix
import           Control.Arrow.Trans
import           Control.Arrow.Reader as Reader
import           Control.Arrow.State
import           Control.Arrow.Transformer.Reader

import           Control.Category

import           Data.Label
import           Data.CallString
import           Data.Profunctor
import           Data.Profunctor.Unsafe((.#))
import           Data.Coerce

-- | Records the full call string.
newtype ContourT lab c a b = ContourT (ReaderT [lab] c a b)
  deriving (Profunctor,Category,Arrow,ArrowLift,ArrowChoice, ArrowState s,
            ArrowEnv var val, ArrowClosure var val env, ArrowFail e, ArrowExcept e)

-- | Runs a computation that records a the full call string of the interpreter.
runContourT :: (Arrow c, Profunctor c) => ContourT lab c a b -> c a b
runContourT (ContourT (ReaderT f)) = lmap (\a -> ([],a)) f
{-# INLINE runContourT #-}

instance ArrowRun c => ArrowRun (ContourT lab c) where
  type Rep (ContourT lab c) x y = Rep c x y
  run = run . runContourT
  {-# INLINE run #-}

type instance Fix x y (ContourT lab c) = ContourT lab (Fix x y c)
instance (ArrowFix x y c, ArrowApply c, HasLabel x lab, Profunctor c) => ArrowFix x y (ContourT lab c) where
  -- Pushes the label of the last argument on the call string
  fix f = ContourT $ ReaderT $ proc (c,x) -> fix (unwrap c . f . wrap) -<< x
    where
      wrap :: (Arrow c, Profunctor c) => c x y -> ContourT lab c x y
      wrap = lift'

      unwrap :: (HasLabel x lab, Arrow c, Profunctor c) => [lab] -> ContourT lab c x y -> c x y
      unwrap c (ContourT (ReaderT f')) = proc x -> do
        y <- f' -< (label x:c,x)
        returnA -< y

instance (ArrowApply c, Profunctor c) => ArrowApply (ContourT lab c) where
  app = ContourT $ app .# first coerce

instance ArrowReader r c => ArrowReader r (ContourT lab c) where
  ask = lift' Reader.ask
  local (ContourT (ReaderT f)) = ContourT $ ReaderT $ ((\(c,(r,x)) -> (r,(c,x))) ^>> Reader.local f)
