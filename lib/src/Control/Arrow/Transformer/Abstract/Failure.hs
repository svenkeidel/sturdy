{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Control.Arrow.Transformer.Abstract.Failure(FailureT(..)) where

import Prelude hiding (id,(.),lookup)

import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Deduplicate
import Control.Arrow.Environment as Env
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Lift
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Except as Exc
import Control.Category

import Data.Abstract.Failure
import Data.Order
import Data.Monoidal
import Data.Identifiable

-- | Describes computations that can fail.
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

instance (ArrowChoice c, ArrowLoop c) => ArrowLoop (FailureT e c) where
  loop (FailureT f) = FailureT $ loop $ proc (b,d) -> do
    e <- f -< (b,d)
    case e of
      Fail e' -> returnA -< (Fail e',d)
      Success (c,d') -> returnA -< (Success c,d')

instance (ArrowChoice c, ArrowState s c) => ArrowState s (FailureT e c) where
  get = lift get
  put = lift put

instance ArrowChoice c => ArrowFail e (FailureT e c) where
  fail = FailureT (arr Fail)

instance (ArrowChoice c, ArrowReader r c) => ArrowReader r (FailureT e c) where
  ask = lift ask
  local (FailureT f) = FailureT (local f)

instance (ArrowChoice c, ArrowEnv var val env c) => ArrowEnv var val env (FailureT e c) where
  type Join (FailureT e c) x y = Env.Join c x (Error e y)
  lookup (FailureT f) (FailureT g) = FailureT $ lookup f g
  getEnv = lift getEnv
  extendEnv = lift extendEnv
  localEnv (FailureT f) = FailureT (localEnv f)

type instance Fix x y (FailureT e c) = FailureT e (Fix x (Error e y) c)
instance (ArrowChoice c, ArrowFix x (Error e y) c) => ArrowFix x y (FailureT e c) where
  fix = liftFix' runFailureT FailureT

instance (ArrowExcept e c, ArrowChoice c) => ArrowExcept e (FailureT e' c) where
  type Join (FailureT e' c) x y = Exc.Join c x (Error e' y)
  throw = lift throw
  catch (FailureT f) (FailureT g) = FailureT (catch f g)
  finally (FailureT f) (FailureT g) = FailureT (finally f g)

instance (Identifiable e, ArrowChoice c, ArrowDeduplicate x (Error e y) c) => ArrowDeduplicate x y (FailureT e c) where
  dedup (FailureT f) = FailureT (dedup f)

instance (ArrowChoice c, ArrowConst x c) => ArrowConst x (FailureT e c) where
  askConst = lift askConst

deriving instance PreOrd (c x (Error e y)) => PreOrd (FailureT e c x y)
deriving instance LowerBounded (c x (Error e y)) => LowerBounded (FailureT e c x y)
deriving instance Complete (c x (Error e y)) => Complete (FailureT e c x y)
deriving instance CoComplete (c x (Error e y)) => CoComplete (FailureT e c x y)
deriving instance UpperBounded (c x (Error e y)) => UpperBounded (FailureT e c x y)
