{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Concrete.Store where

import           Prelude hiding ((.))

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Fail
import           Control.Arrow.Except
import           Control.Arrow.Fix
import           Control.Arrow.Trans
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Store
import           Control.Arrow.Transformer.State
import           Control.Arrow.Utils
import           Control.Category

import           Data.Profunctor
import           Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as S
import           Data.Identifiable

-- | Arrow transformer that adds a store to a computation.
newtype StoreT var val c x y = StoreT (StateT (HashMap var val) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans,ArrowLift,
            ArrowConst r, ArrowReader r, ArrowFail e, ArrowExcept e)

-- | Execute a computation and only return the result value and store.
runStoreT :: StoreT var val c x y -> c (HashMap var val, x) (HashMap var val, y)
runStoreT (StoreT (StateT f)) = f

-- | Execute a computation and only return the result value.
evalStoreT :: Arrow c => StoreT var val c x y -> c (HashMap var val, x) y
evalStoreT f = runStoreT f >>> pi2

-- | Execute a computation and only return the result store.
execStoreT :: Arrow c => StoreT var val c x y -> c (HashMap var val, x) (HashMap var val)
execStoreT f = runStoreT f >>> pi1

instance (Identifiable var, ArrowChoice c, Profunctor c) => ArrowStore var val (StoreT var val c) where
  type Join (StoreT var val c) x y = ()
  read (StoreT f) (StoreT g) = StoreT $ proc (var,x) -> do
    s <- get -< ()
    case S.lookup var s of
      Just v -> f -< (v,x)
      Nothing -> g -< x
  write = StoreT $ modify $ arr (\((x,v),s) -> ((),S.insert x v s))

instance ArrowState s c => ArrowState s (StoreT var val c) where
  get = lift' get
  put = lift' put

instance (ArrowApply c,Profunctor c) => ArrowApply (StoreT var val c) where app = StoreT ((\(StoreT f,x) -> (f,x)) ^>> app)

type instance Fix x y (StoreT var val c) = StoreT var val (Fix (Dom (StoreT var val) x y) (Cod (StoreT var val) x y) c)
deriving instance ArrowFix (Dom (StoreT var val) x y) (Cod (StoreT var val) x y) c => ArrowFix x y (StoreT var val c)
