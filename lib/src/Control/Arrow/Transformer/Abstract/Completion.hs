{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Completion(CompletionT(..)) where

import Prelude hiding ((.),id,lookup,fail)

import Control.Arrow
import Control.Arrow.Deduplicate
import Control.Arrow.Environment as Env
import Control.Arrow.Fail
import Control.Arrow.Lift
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Except as Exc
import Control.Arrow.Fix
import Control.Arrow.Abstract.Join
import Control.Category

import Data.Abstract.FreeCompletion
import Data.Monoidal
import Data.Order hiding (lub)

-- | Allows to describe computations over non-completely ordered types.
-- E.g. allows to join a computation of type 'c x [y]'.
newtype CompletionT c x y = CompletionT { runCompletionT :: c x (FreeCompletion y) }

instance ArrowLift CompletionT where
  lift f = CompletionT (f >>> arr Lower)

instance ArrowChoice c => Category (CompletionT c) where
  id = lift id
  CompletionT f . CompletionT g = CompletionT $ proc x -> do
    g' <- g -< x
    case g' of
      Lower a -> f -< a
      Top -> returnA -< Top

instance ArrowChoice c => Arrow (CompletionT c) where
  arr = lift . arr
  first (CompletionT f) = CompletionT $ first f >>^ strength1
  second (CompletionT f) = CompletionT $ second f >>^ strength2

instance ArrowChoice c => ArrowChoice (CompletionT c) where
  left (CompletionT f) = CompletionT $ left f >>^ strength1
  right (CompletionT f) = CompletionT $ right f >>^ strength2

instance (ArrowApply c, ArrowChoice c) => ArrowApply (CompletionT c) where
  app = CompletionT $ first runCompletionT ^>> app

instance (ArrowChoice c, ArrowState s c) => ArrowState s (CompletionT c) where
  get = lift get
  put = lift put

instance (ArrowChoice c, ArrowFail e c) => ArrowFail e (CompletionT c) where
  fail = lift fail

instance (ArrowChoice c, ArrowReader r c) => ArrowReader r (CompletionT c) where
  ask = lift ask
  local (CompletionT f) = CompletionT (local f)

instance (ArrowChoice c, ArrowEnv var val env c) => ArrowEnv var val env (CompletionT c) where
  type Join (CompletionT c) x y = Env.Join c x (FreeCompletion y)
  lookup (CompletionT f) (CompletionT g) = CompletionT (lookup f g)
  getEnv = lift getEnv
  extendEnv = lift extendEnv
  localEnv (CompletionT f) = CompletionT (localEnv f)

instance (ArrowChoice c, ArrowExcept e c) => ArrowExcept e (CompletionT c) where
  type Join (CompletionT c) x y = Exc.Join c x (FreeCompletion y)
  throw = lift throw
  catch (CompletionT f) (CompletionT g) = CompletionT $ catch f g
  finally (CompletionT f) (CompletionT g) = CompletionT $ finally f g

instance ArrowChoice c => ArrowDeduplicate x y (CompletionT c) where
  dedup = returnA

type instance Fix x y (CompletionT c) = CompletionT (Fix x (FreeCompletion y) c)
instance (ArrowChoice c, ArrowFix x (FreeCompletion y) c) => ArrowFix x y (CompletionT c) where
  fix = liftFix' runCompletionT CompletionT

instance (ArrowChoice c, ArrowJoin c) => ArrowJoin (CompletionT c) where
  joinWith lub (CompletionT f) (CompletionT g) = CompletionT $ joinWith join f g
    where join (Lower x) (Lower y) = Lower (lub x y)
          join Top _ = Top
          join _ Top = Top

deriving instance PreOrd (c x (FreeCompletion y)) => PreOrd (CompletionT c x y)
deriving instance LowerBounded (c x (FreeCompletion y)) => LowerBounded (CompletionT c x y)
deriving instance Complete (c x (FreeCompletion y)) => Complete (CompletionT c x y)
deriving instance CoComplete (c x (FreeCompletion y)) => CoComplete (CompletionT c x y)
deriving instance UpperBounded (c x (FreeCompletion y)) => UpperBounded (CompletionT c x y)
