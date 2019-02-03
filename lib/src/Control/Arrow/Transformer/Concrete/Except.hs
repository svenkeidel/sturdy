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
import Control.Arrow.Utils
import Control.Category

import Data.Concrete.Error
import Data.Monoidal
import Data.Identifiable
import Data.Profunctor

-- | Arrow transformer that adds exceptions to the result of a computation
newtype ExceptT e c x y = ExceptT { runExceptT :: c x (Error e y) }

instance (ArrowChoice c, Profunctor c) => ArrowExcept e (ExceptT e c) where
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

instance (Profunctor c, Arrow c) => Profunctor (ExceptT e c) where
  dimap f g h = lift $ dimap f (fmap g) (unlift h)
  lmap f h = lift $ lmap f (unlift h)
  rmap g h = lift $ rmap (fmap g) (unlift h)

instance ArrowTrans (ExceptT e) where
  type Dom (ExceptT e) x y = x
  type Cod (ExceptT e) x y = Error e y
  lift = ExceptT
  unlift = runExceptT

instance ArrowLift (ExceptT e) where
  lift' f = lift (rmap Success f)

instance (ArrowChoice c, Profunctor c) => Category (ExceptT r c) where
  id = lift' id
  f . g = lift $ unlift g >>> lmap toEither (arr Fail ||| unlift f)

instance (ArrowChoice c, Profunctor c) => Arrow (ExceptT r c) where
  arr f = lift' (arr f)
  first f = lift $ rmap strength1 (first (unlift f))
  second f = lift $ rmap strength2 (second (unlift f))
  f *** g = first f >>> second g
  f &&& g = lmap duplicate (f *** g)

instance (ArrowChoice c, Profunctor c) => ArrowChoice (ExceptT r c) where
  left f = lift $ left (unlift f) >>^ strength1
  right f = lift $ right (unlift f) >>^ strength2
  f ||| g = lift $ unlift f ||| unlift g
  f +++ g = lift $ rmap distribute2 (unlift f +++ unlift g)

instance (ArrowChoice c, ArrowApply c, Profunctor c) => ArrowApply (ExceptT e c) where
  app = lift $ lmap (first unlift) app

instance (ArrowChoice c, ArrowState s c) => ArrowState s (ExceptT e c) where
  get = lift' get
  put = lift' put

instance (ArrowChoice c, ArrowFail f c) => ArrowFail f (ExceptT e c) where
  fail = lift' fail

instance (ArrowChoice c, ArrowReader r c) => ArrowReader r (ExceptT e c) where
  ask = lift' ask
  local f = lift (local (unlift f))

instance (ArrowChoice c, ArrowEnv x y env c) => ArrowEnv x y env (ExceptT e c) where
  type Join (ExceptT e c) x y = Env.Join c (Dom (ExceptT e) x y) (Cod (ExceptT e) x y)
  lookup f g = lift $ lookup (unlift f) (unlift g)
  getEnv = lift' getEnv
  extendEnv = lift' extendEnv
  localEnv f = lift (localEnv (unlift f))

instance (ArrowChoice c, ArrowStore var val c) => ArrowStore var val (ExceptT e c) where
  type Join (ExceptT e c) x y = Store.Join c (Dom (ExceptT e) x y) (Cod (ExceptT e) x y)
  read f g = lift $ read (unlift f) (unlift g)
  write = lift' write

type instance Fix x y (ExceptT e c) = ExceptT e (Fix (Dom (ExceptT e) x y) (Cod (ExceptT e) x y) c)
instance (ArrowChoice c, ArrowFix (Dom (ExceptT e) x y) (Cod (ExceptT e) x y) c) => ArrowFix x y (ExceptT e c) where
  fix = liftFix

instance (Identifiable e, ArrowChoice c, ArrowDeduplicate x (Error e y) c) => ArrowDeduplicate x y (ExceptT e c) where
  dedup f = lift (dedup (unlift f))

instance (ArrowChoice c, ArrowConst r c) => ArrowConst r (ExceptT e c) where
  askConst = lift' askConst
