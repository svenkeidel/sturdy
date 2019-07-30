{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Abstract.FlowInsensitive.Store where

import           Prelude

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Except
import           Control.Arrow.Fix
import           Control.Arrow.Fail
import           Control.Arrow.Order
import           Control.Arrow.Trans
import           Control.Arrow.State
import           Control.Arrow.Store
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Abstract.MonotoneState

import           Data.Coerce
import           Data.Identifiable
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Order
import           Data.Profunctor
import           Data.Profunctor.Unsafe((.#))

type Store var val = HashMap var val
newtype StoreT var val c x y = StoreT (MonotoneStateT (Store var val) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans,ArrowLift,ArrowRun,ArrowEffectCommutative,ArrowFail e,ArrowExcept e)

runStoreT :: StoreT var val c x y -> c (Store var val, x) (Store var val, y)
runStoreT = coerce
{-# INLINE runStoreT #-}

instance (Identifiable var, Complete val, ArrowChoice c, ArrowEffectCommutative c) => ArrowStore var val (StoreT var val c) where
  type Join y (StoreT var val c) = (Complete y)
  read (StoreT f) (StoreT g) = StoreT $ proc (var,x) -> do
    s <- get -< ()
    case M.lookup var s of
      Just val -> f -< (val,x)
      Nothing  -> g -< x
  write = StoreT $ modify $ arr $ \((var,val),st) -> ((),M.insertWith (⊔) var val st)

instance ArrowState s c => ArrowState s (StoreT var val c) where
  get = lift' get
  put = lift' put

deriving instance (Complete y, ArrowEffectCommutative c) => ArrowComplete y (StoreT var val c)
deriving instance (ArrowEffectCommutative c) => ArrowJoin (StoreT var val c)
instance (ArrowApply c, Profunctor c) => ArrowApply (StoreT var val c) where
  app = StoreT (app .# first coerce)

type instance Fix x y (StoreT var val c) = StoreT var val (Fix (Dom (StoreT var val) x y) (Cod (StoreT var val) x y) c)
deriving instance ArrowFix (Dom (StoreT var val) x y) (Cod (StoreT var val) x y) c => ArrowFix x y (StoreT var val c)
