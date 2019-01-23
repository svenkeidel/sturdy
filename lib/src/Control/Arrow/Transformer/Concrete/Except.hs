{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Concrete.Except(ExceptT(..)) where

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
import Control.Arrow.Except
import Control.Category

import Data.Concrete.Error
import Data.Monoidal
import Data.Identifiable

-- | Arrow transformer that adds exceptions to the result of a computation
newtype ExceptT e c x y = ExceptT { runExceptT :: c x (Error e y) }

instance ArrowLift (ExceptT e) where
  lift f = ExceptT (f >>> arr Success)

instance ArrowChoice c => Category (ExceptT r c) where
  id = lift id
  ExceptT f . ExceptT g = ExceptT $ g >>> toEither ^>> arr Fail ||| f

instance ArrowChoice c => Arrow (ExceptT r c) where
  arr f = lift (arr f)
  first (ExceptT f) = ExceptT $ first f >>^ strength1
  second (ExceptT f) = ExceptT $ second f >>^ strength2

instance ArrowChoice c => ArrowChoice (ExceptT r c) where
  left (ExceptT f) = ExceptT $ left f >>^ strength1
  right (ExceptT f) = ExceptT $ right f >>^ strength2
  ExceptT f ||| ExceptT g = ExceptT (f ||| g)
  ExceptT f +++ ExceptT g = ExceptT $ f +++ g >>^ from distribute

instance (ArrowChoice c, ArrowApply c) => ArrowApply (ExceptT e c) where
  app = ExceptT $ first runExceptT ^>> app

instance (ArrowChoice c, ArrowState s c) => ArrowState s (ExceptT e c) where
  get = lift get
  put = lift put

instance (ArrowChoice c, ArrowFail f c) => ArrowFail f (ExceptT e c) where
  fail = lift fail

instance (ArrowChoice c, ArrowReader r c) => ArrowReader r (ExceptT e c) where
  ask = lift ask
  local (ExceptT f) = ExceptT (local f)

instance (ArrowChoice c, ArrowEnv x y env c) => ArrowEnv x y env (ExceptT e c) where
  type Join (ExceptT e c) x y = Env.Join c x (Error e y)
  lookup (ExceptT f) (ExceptT g) = ExceptT $ lookup f g
  getEnv = lift getEnv
  extendEnv = lift extendEnv
  localEnv (ExceptT f) = ExceptT (localEnv f)

instance (ArrowChoice c, ArrowStore var val c) => ArrowStore var val (ExceptT e c) where
  type Join (ExceptT e c) x y = Store.Join c x (Error e y)
  read (ExceptT f) (ExceptT g) = ExceptT $ read f g
  write = lift write

type instance Fix x y (ExceptT e c) = ExceptT e (Fix x (Error e y) c)
instance (ArrowChoice c, ArrowFix x (Error e y) c) => ArrowFix x y (ExceptT e c) where
  fix = liftFix' runExceptT ExceptT

instance ArrowChoice c => ArrowExcept e (ExceptT e c) where
  type Join (ExceptT e c) x y = ()

  throw = ExceptT $ arr Fail

  catch (ExceptT f) (ExceptT g) = ExceptT $ proc x -> do
    e <- f -< x
    case e of
      Fail er -> g -< (x,er)
      Success y -> returnA -< Success y

  finally (ExceptT f) (ExceptT g) = ExceptT $ proc x -> do
    e <- f -< x
    g -< x
    returnA -< e

instance (Identifiable e, ArrowChoice c, ArrowDeduplicate x (Error e y) c) => ArrowDeduplicate x y (ExceptT e c) where
  dedup (ExceptT f) = ExceptT (dedup f)

instance (ArrowChoice c, ArrowConst r c) => ArrowConst r (ExceptT e c) where
  askConst = lift askConst
