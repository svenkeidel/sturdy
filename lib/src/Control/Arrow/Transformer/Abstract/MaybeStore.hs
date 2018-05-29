{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Abstract.MaybeStore where

import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Lift
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.MaybeStore
import Control.Arrow.Transformer.State
import Control.Arrow.Utils
import Control.Category

import Data.Abstract.Error
import Data.Abstract.Store (Store)
import qualified Data.Abstract.Store as S
import Data.Order
import Data.Identifiable

newtype MaybeStoreArrow var val c x y = MaybeStoreArrow (State (Store var val) c x y)

runMaybeStore :: MaybeStoreArrow var val c x y -> c (Store var val, x) (Store var val, y)
runMaybeStore (MaybeStoreArrow (State f)) = f

evalMaybeStore :: Arrow c => MaybeStoreArrow var val c x y -> c (Store var val, x) y
evalMaybeStore f = runMaybeStore f >>> pi2

execMaybeStore :: Arrow c => MaybeStoreArrow var val c x y -> c (Store var val, x) (Store var val)
execMaybeStore f = runMaybeStore f >>> pi1

instance ArrowConst r c => ArrowConst r (MaybeStoreArrow var val c) where
  askConst = lift askConst

instance (Identifiable var, ArrowChoice c, Complete (c ((Store var val,Maybe val),Store var val) (Store var val,Maybe val))) =>
  ArrowMaybeStore var val (MaybeStoreArrow var val c) where
  read =
    MaybeStoreArrow $ State $ proc (s,var) -> case S.lookup var s of
      Success v -> returnA -< (s,(Just v))
      Fail _ -> returnA -< (s,Nothing)
  write = MaybeStoreArrow (State (arr (\(s,(x,v)) -> (S.insert x v s,()))))

instance ArrowState s c => ArrowState s (MaybeStoreArrow var val c) where
  getA = lift getA
  putA = lift putA

deriving instance Category c => Category (MaybeStoreArrow var val c)
deriving instance Arrow c => Arrow (MaybeStoreArrow var val c)
deriving instance ArrowChoice c => ArrowChoice (MaybeStoreArrow var val c)
deriving instance ArrowLift (MaybeStoreArrow var val)
deriving instance ArrowReader r c => ArrowReader r (MaybeStoreArrow var val c)
deriving instance ArrowFail e c => ArrowFail e (MaybeStoreArrow var val c)
deriving instance ArrowLoop c => ArrowLoop (MaybeStoreArrow var val c)
instance ArrowApply c => ArrowApply (MaybeStoreArrow var val c) where app = MaybeStoreArrow $ (\(MaybeStoreArrow f,x) -> (f,x)) ^>> app
type instance Fix x y (MaybeStoreArrow var val c) = MaybeStoreArrow var val (Fix (Store var val,x) (Store var val,y) c)
deriving instance ArrowFix (Store var val, x) (Store var val, y) c => ArrowFix x y (MaybeStoreArrow var val c)
deriving instance PreOrd (c (Store var val,x) (Store var val,y)) => PreOrd (MaybeStoreArrow var val c x y)
deriving instance Complete (c (Store var val,x) (Store var val,y)) => Complete (MaybeStoreArrow var val c x y)
deriving instance CoComplete (c (Store var val,x) (Store var val,y)) => CoComplete (MaybeStoreArrow var val c x y)
deriving instance UpperBounded (c (Store var val,x) (Store var val,y)) => UpperBounded (MaybeStoreArrow var val c x y)
deriving instance LowerBounded (c (Store var val,x) (Store var val,y)) => LowerBounded (MaybeStoreArrow var val c x y)
