{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Concrete.Except(ExceptT,runExceptT) where

import Prelude hiding (id,(.))

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
import Control.Arrow.Transformer.Kleisli

import Data.Concrete.Error
import Data.Identifiable
import Data.Profunctor

-- | Arrow transformer that adds exceptions to the result of a computation
newtype ExceptT e c x y = ExceptT { unExceptT :: KleisliT (Error e) c x y }

runExceptT :: ExceptT e c x y -> c x (Error e y)
runExceptT = runKleisliT . unExceptT

instance (ArrowChoice c, Profunctor c) => ArrowExcept e (ExceptT e c) where
  type Join (ExceptT e c) x y = ()

  throw = lift $ arr Fail

  try f g h = lift $ proc x -> do
    e <- unlift f -< x
    case e of
      Success y -> unlift g -< y
      Fail er -> unlift h -< (x,er)

  finally f g = lift $ proc x -> do
    e <- unlift f -< x
    unlift g -< x
    returnA -< e

deriving instance (ArrowChoice c, Profunctor c) => Profunctor (ExceptT e c)
deriving instance (ArrowChoice c, Profunctor c) => Category (ExceptT e c)
deriving instance (ArrowChoice c, Profunctor c) => Arrow (ExceptT e c)
deriving instance (ArrowChoice c, Profunctor c) => ArrowChoice (ExceptT e c)
instance (ArrowChoice c, ArrowApply c, Profunctor c) => ArrowApply (ExceptT e c) where app = lift $ lmap (first unlift) app
deriving instance ArrowTrans (ExceptT e)
deriving instance ArrowLift (ExceptT e)
deriving instance (ArrowChoice c, ArrowState s c) => ArrowState s (ExceptT e c)
deriving instance (ArrowChoice c, ArrowFail f c) => ArrowFail f (ExceptT e c)
deriving instance (ArrowChoice c, ArrowReader r c) => ArrowReader r (ExceptT e c)
deriving instance (ArrowChoice c, ArrowEnv x y env c) => ArrowEnv x y env (ExceptT e c)
deriving instance (ArrowChoice c, ArrowStore var val c) => ArrowStore var val (ExceptT e c)
type instance Fix x y (ExceptT e c) = ExceptT e (Fix (Dom (ExceptT e) x y) (Cod (ExceptT e) x y) c)
deriving instance (ArrowChoice c, ArrowFix (Dom (ExceptT e) x y) (Cod (ExceptT e) x y) c) => ArrowFix x y (ExceptT e c)
deriving instance (Identifiable (Error e y), ArrowChoice c, ArrowDeduplicate x (Error e y) c) => ArrowDeduplicate x y (ExceptT e c)
deriving instance (ArrowChoice c, ArrowConst r c) => ArrowConst r (ExceptT e c)
