{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Concrete.MaybeStore where

import Prelude hiding ((.),read)

import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Lift
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.MaybeStore
import Control.Arrow.TryCatch
import Control.Arrow.Transformer.State
import Control.Arrow.Utils
import Control.Category

import Data.Concrete.Error
import Data.Concrete.Store (Store)
import qualified Data.Concrete.Store as S
import Data.Identifiable

newtype MaybeStoreArrow var val c x y = MaybeStoreArrow (State (Store var val) c x y)

runMaybeStore :: MaybeStoreArrow var val c x y -> c (Store var val, x) (Store var val, y)
runMaybeStore (MaybeStoreArrow (State f)) = f

evalMaybeStore :: Arrow c => MaybeStoreArrow var val c x y -> c (Store var val, x) y
evalMaybeStore f = runMaybeStore f >>> pi2

execMaybeStore :: Arrow c => MaybeStoreArrow var val c x y -> c (Store var val, x) (Store var val)
execMaybeStore f = runMaybeStore f >>> pi1

instance ArrowLift (MaybeStoreArrow var val) where
  lift f = MaybeStoreArrow (lift f)

instance (Identifiable var, ArrowChoice c) =>
  ArrowMaybeStore var val (MaybeStoreArrow var val c) where
  read =
    MaybeStoreArrow $ State $ proc (s,var) -> case S.lookup var s of
      Success v -> returnA -< (s,Just v)
      Fail _ -> returnA -< (s,Nothing)
  write = MaybeStoreArrow (State (arr (\(s,(x,v)) -> (S.insert x v s,()))))

instance ArrowState s c => ArrowState s (MaybeStoreArrow var val c) where
  getA = lift getA
  putA = lift putA

instance ArrowTryCatch (Store var val,e) (Store var val,x) (Store var val,y) (Store var val,z) c =>
  ArrowTryCatch e x y z (MaybeStoreArrow var val c) where
    tryCatchA (MaybeStoreArrow f) (MaybeStoreArrow g) (MaybeStoreArrow h) = MaybeStoreArrow $ tryCatchA f g h

deriving instance Category c => Category (MaybeStoreArrow var val c)
deriving instance Arrow c => Arrow (MaybeStoreArrow var val c)
deriving instance ArrowChoice c => ArrowChoice (MaybeStoreArrow var val c)
deriving instance ArrowReader r c => ArrowReader r (MaybeStoreArrow var val c)
deriving instance ArrowFail e c => ArrowFail e (MaybeStoreArrow var val c)
deriving instance ArrowConst r c => ArrowConst r (MaybeStoreArrow var val c)

type instance Fix x y (MaybeStoreArrow var val c) = MaybeStoreArrow var val (Fix (Store var val,x) (Store var val,y) c)
deriving instance ArrowFix (Store var val, x) (Store var val, y) c => ArrowFix x y (MaybeStoreArrow var val c)
