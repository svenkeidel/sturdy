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
import Control.Arrow.Trans
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

instance ArrowChoice c => ArrowExcept e (ExceptT e c) where
  type Join (ExceptT e c) x y = ()

  throw = lift $ arr Fail

  catch f g = lift $ proc x -> do
    e <- unlift f -< x
    case e of
      Fail er -> unlift g -< (x,er)
      Success y -> returnA -< Success y

  finally f g = lift $ proc x -> do
    e <- unlift f -< x
    unlift g -< x
    returnA -< e

instance ArrowTrans (ExceptT e) where
  type Dom1 (ExceptT e) x y = x
  type Cod1 (ExceptT e) x y = Error e y
  lift = ExceptT
  unlift = runExceptT

instance ArrowLift (ExceptT e) where
  lift' f = ExceptT (f >>> arr Success)

instance ArrowChoice c => Category (ExceptT r c) where
  id = lift' id
  f . g = lift $ unlift g >>> toEither ^>> arr Fail ||| unlift f

instance ArrowChoice c => Arrow (ExceptT r c) where
  arr f = lift' (arr f)
  first f = lift $ first (unlift f) >>^ strength1
  second f = lift $ second (unlift f) >>^ strength2

instance ArrowChoice c => ArrowChoice (ExceptT r c) where
  left f = lift $ left (unlift f) >>^ strength1
  right f = lift $ right (unlift f) >>^ strength2
  f ||| g = lift (unlift f ||| unlift g)
  f +++ g = lift $ unlift f +++ unlift g >>^ from distribute

instance (ArrowChoice c, ArrowApply c) => ArrowApply (ExceptT e c) where
  app = lift $ first unlift ^>> app

instance (ArrowChoice c, ArrowState s c) => ArrowState s (ExceptT e c) where
  get = lift' get
  put = lift' put

instance (ArrowChoice c, ArrowFail f c) => ArrowFail f (ExceptT e c) where
  fail = lift' fail

instance (ArrowChoice c, ArrowReader r c) => ArrowReader r (ExceptT e c) where
  ask = lift' ask
  local f = lift (local (unlift f))

instance (ArrowChoice c, ArrowEnv x y env c) => ArrowEnv x y env (ExceptT e c) where
  type Join (ExceptT e c) x y = Env.Join c (Dom1 (ExceptT e) x y) (Cod1 (ExceptT e) x y)
  lookup f g = lift $ lookup (unlift f) (unlift g)
  getEnv = lift' getEnv
  extendEnv = lift' extendEnv
  localEnv f = lift (localEnv (unlift f))

instance (ArrowChoice c, ArrowStore var val c) => ArrowStore var val (ExceptT e c) where
  type Join (ExceptT e c) x y = Store.Join c (Dom1 (ExceptT e) x y) (Cod1 (ExceptT e) x y)
  read f g = lift $ read (unlift f) (unlift g)
  write = lift' write

instance (ArrowChoice c, ArrowFix (Dom1 (ExceptT e) x y) (Cod1 (ExceptT e) x y) c) => ArrowFix x y (ExceptT e c) where
  fix = liftFix

instance (Identifiable e, ArrowChoice c, ArrowDeduplicate x (Error e y) c) => ArrowDeduplicate x y (ExceptT e c) where
  dedup f = lift (dedup (unlift f))

instance (ArrowChoice c, ArrowConst r c) => ArrowConst r (ExceptT e c) where
  askConst = lift' askConst
