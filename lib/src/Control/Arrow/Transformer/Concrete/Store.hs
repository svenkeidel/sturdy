{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Concrete.Store where

import           Prelude hiding ((.))

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Store
import           Control.Arrow.Trans
import           Control.Arrow.Transformer.State
import           Control.Arrow.Utils

import           Data.Profunctor.Unsafe
import           Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as S
import           Data.Identifiable
import           Data.Coerce

-- | Arrow transformer that adds a store to a computation.
newtype StoreT store c x y = StoreT (StateT store c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowLift,ArrowTrans,ArrowRun,
            ArrowConst r, ArrowReader r, ArrowFail e, ArrowExcept e, ArrowState store)

-- | Execute a computation and only return the result value and store.
runStoreT :: StoreT store c x y -> c (store, x) (store, y)
runStoreT = coerce
{-# INLINE runStoreT #-}

-- | Execute a computation and only return the result value.
evalStoreT :: (Profunctor c) => StoreT store c x y -> c (store, x) y
evalStoreT f = rmap pi2 (runStoreT f)

-- | Execute a computation and only return the result store.
execStoreT :: (Profunctor c) => StoreT store c x y -> c (store, x) store
execStoreT f = rmap pi1 (runStoreT f)

instance (Identifiable var, ArrowChoice c, Profunctor c) => ArrowStore var val (StoreT (HashMap var val) c) where
  type Join y (StoreT (HashMap var val) c) = ()
  read (StoreT f) (StoreT g) = StoreT $ proc (var,x) -> do
    s <- get -< ()
    case S.lookup var s of
      Just v -> f -< (v,x)
      Nothing -> g -< x
  write = StoreT $ modify $ arr (\((x,v),s) -> ((),S.insert x v s))

instance (ArrowApply c,Profunctor c) => ArrowApply (StoreT store c) where
  app = StoreT (app .# first coerce)
  {-# INLINE app #-}

instance ArrowFix (Underlying (StoreT store c) x y) => ArrowFix (StoreT store c x y) where
  type Fix (StoreT store c x y) = Fix (Underlying (StoreT store c) x y)

