{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Concrete.Failure(FailureT(..)) where

import Prelude hiding (id,(.),lookup,read,fail)

import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Deduplicate
import Control.Arrow.Environment as Env
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Lift
import Control.Arrow.Reader
import Control.Arrow.Store as Store
import Control.Arrow.State
import Control.Arrow.Except as Exc
import Control.Category

import Data.Concrete.Error
import Data.Monoidal
import Data.Identifiable

-- | Arrow transformer that adds failure to the result of a computation
newtype FailureT e c x y = FailureT { runFailureT :: c x (Error e y) }

instance ArrowLift (FailureT e) where
  lift f = FailureT (f >>> arr Success)

instance ArrowChoice c => Category (FailureT r c) where
  id = lift id
  FailureT f . FailureT g = FailureT $ g >>> toEither ^>> arr Fail ||| f

instance ArrowChoice c => Arrow (FailureT r c) where
  arr f = lift (arr f)
  first (FailureT f) = FailureT $ first f >>^ strength1
  second (FailureT f) = FailureT $ second f >>^ strength2

instance ArrowChoice c => ArrowChoice (FailureT r c) where
  left (FailureT f) = FailureT $ left f >>^ strength1
  right (FailureT f) = FailureT $ right f >>^ strength2
  FailureT f ||| FailureT g = FailureT (f ||| g)
  FailureT f +++ FailureT g = FailureT $ f +++ g >>^ from distribute

instance (ArrowChoice c, ArrowApply c) => ArrowApply (FailureT e c) where
  app = FailureT $ first runFailureT ^>> app

instance (ArrowChoice c, ArrowState s c) => ArrowState s (FailureT e c) where
  get = lift get
  put = lift put

instance (ArrowChoice c, ArrowReader r c) => ArrowReader r (FailureT e c) where
  ask = lift ask
  local (FailureT f) = FailureT (local f)

instance (ArrowChoice c, ArrowEnv x y env c) => ArrowEnv x y env (FailureT e c) where
  type Join (FailureT e c) x y = Env.Join c x (Error e y)
  lookup (FailureT f) (FailureT g) = FailureT $ lookup f g
  getEnv = lift getEnv
  extendEnv = lift extendEnv
  localEnv (FailureT f) = FailureT (localEnv f)

instance (ArrowChoice c, ArrowStore var val c) => ArrowStore var val (FailureT e c) where
  type Join (FailureT e c) x y = Store.Join c x (Error e y)
  read (FailureT f) (FailureT g) = FailureT $ read f g
  write = lift write

type instance Fix x y (FailureT e c) = FailureT e (Fix x (Error e y) c)
instance (ArrowChoice c, ArrowFix x (Error e y) c) => ArrowFix x y (FailureT e c) where
  fix = liftFix' runFailureT FailureT

instance ArrowChoice c => ArrowFail e (FailureT e c) where
  fail = FailureT $ arr Fail

instance (ArrowChoice c, ArrowExcept e c) => ArrowExcept e (FailureT e' c) where
  type Join (FailureT e' c) x y = Exc.Join c x (Error e' y)
  throw = lift throw
  catch (FailureT f) (FailureT g) = FailureT $ catch f g
  finally (FailureT f) (FailureT g) = FailureT $ finally f g

instance (Identifiable e, ArrowChoice c, ArrowDeduplicate x (Error e y) c) => ArrowDeduplicate x y (FailureT e c) where
  dedup (FailureT f) = FailureT (dedup f)

instance (ArrowChoice c, ArrowConst r c) => ArrowConst r (FailureT e c) where
  askConst = lift askConst
