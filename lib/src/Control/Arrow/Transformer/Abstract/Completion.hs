{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Completion(CompletionT(..)) where

import Prelude hiding ((.),id,lookup,fail)

import Control.Applicative
import Control.Arrow
import Control.Arrow.Deduplicate
import Control.Arrow.Environment as Env
import Control.Arrow.Fail
import Control.Arrow.Trans
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Except as Exc
import Control.Arrow.Fix
import Control.Arrow.Abstract.Join
import Control.Category

import Data.Profunctor
import Data.Abstract.FreeCompletion
import Data.Monoidal
import Data.Order hiding (lub)

-- | Allows to describe computations over non-completely ordered types.
-- E.g. allows to join a computation of type 'c x [y]'.
newtype CompletionT c x y = CompletionT { runCompletionT :: c x (FreeCompletion y) }

instance (Profunctor c, Arrow c) => Profunctor (CompletionT c) where
  dimap f g h = lift $ dimap f (fmap g) (unlift h)
  lmap f h = lift $ lmap f (unlift h)
  rmap g h = lift $ rmap (fmap g) (unlift h)

instance ArrowTrans CompletionT where
  type Dom CompletionT x y = x
  type Cod CompletionT x y = FreeCompletion y
  lift = CompletionT
  unlift = runCompletionT

instance ArrowLift CompletionT where
  lift' f = CompletionT (rmap Lower f)

instance (ArrowChoice c, Profunctor c) => Category (CompletionT c) where
  id = lift' id
  CompletionT f . CompletionT g = CompletionT $ proc x -> do
    g' <- g -< x
    case g' of
      Lower a -> f -< a
      Top -> returnA -< Top

instance (ArrowChoice c, Profunctor c) => Arrow (CompletionT c) where
  arr = lift' . arr
  first f = lift $ rmap strength1 (first (unlift f))
  second f = lift $ rmap strength2 (second (unlift f))
  f *** g = lift $ rmap (uncurry (liftA2 (,))) (unlift f *** unlift g)
  f &&& g = lift $ rmap (uncurry (liftA2 (,))) (unlift f &&& unlift g)

instance (ArrowChoice c, Profunctor c) => ArrowChoice (CompletionT c) where
  left (CompletionT f) = CompletionT $ rmap strength1 (left f)
  right (CompletionT f) = CompletionT $ rmap strength2 (right f)

instance (ArrowApply c, ArrowChoice c, Profunctor c) => ArrowApply (CompletionT c) where
  app = CompletionT $ lmap (first runCompletionT) app

instance (ArrowChoice c, ArrowState s c, Profunctor c) => ArrowState s (CompletionT c) where
  get = lift' get
  put = lift' put

instance (ArrowChoice c, ArrowFail e c) => ArrowFail e (CompletionT c) where
  fail = lift' fail

instance (ArrowChoice c, ArrowReader r c) => ArrowReader r (CompletionT c) where
  ask = lift' ask
  local f = lift (local (unlift f))

instance (ArrowChoice c, ArrowEnv var val env c) => ArrowEnv var val env (CompletionT c) where
  type Join (CompletionT c) x y = Env.Join c (Dom CompletionT x y) (Cod CompletionT x y)
  lookup f g = lift (lookup (unlift f) (unlift g))
  getEnv = lift' getEnv
  extendEnv = lift' extendEnv
  localEnv f = lift (localEnv (unlift f))

instance (ArrowChoice c, ArrowExcept e c) => ArrowExcept e (CompletionT c) where
  type Join (CompletionT c) x y = Exc.Join c (Dom CompletionT x y) (Cod CompletionT x y)
  throw = lift' throw
  catch f g = lift $ catch (unlift f) (unlift g)
  finally f g = lift $ finally (unlift f) (unlift g)

instance (ArrowChoice c, Profunctor c) => ArrowDeduplicate x y (CompletionT c) where
  dedup = returnA

type instance Fix x y (CompletionT c) = CompletionT (Fix (Dom CompletionT x y) (Cod CompletionT x y) c)
instance (ArrowChoice c, ArrowFix (Dom CompletionT x y) (Cod CompletionT x y) c) => ArrowFix x y (CompletionT c) where
  fix = liftFix

instance (ArrowChoice c, ArrowJoin c) => ArrowJoin (CompletionT c) where
  joinWith lub f g = lift $ joinWith join (unlift f) (unlift g)
    where join (Lower x) (Lower y) = Lower (lub x y)
          join Top _ = Top
          join _ Top = Top

deriving instance PreOrd (c x (FreeCompletion y)) => PreOrd (CompletionT c x y)
deriving instance LowerBounded (c x (FreeCompletion y)) => LowerBounded (CompletionT c x y)
deriving instance Complete (c x (FreeCompletion y)) => Complete (CompletionT c x y)
deriving instance CoComplete (c x (FreeCompletion y)) => CoComplete (CompletionT c x y)
deriving instance UpperBounded (c x (FreeCompletion y)) => UpperBounded (CompletionT c x y)
