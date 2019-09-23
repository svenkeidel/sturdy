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

import           Data.Profunctor
import           Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as S
import           Data.Identifiable
import           Data.Coerce

-- | Arrow transformer that adds a store to a computation.
newtype StoreT var val c x y = StoreT (StateT (HashMap var val) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans,ArrowLift,ArrowRun,
            ArrowConst r, ArrowReader r, ArrowFail e, ArrowExcept e, ArrowState (HashMap var val))

-- | Execute a computation and only return the result value and store.
runStoreT :: StoreT var val c x y -> c (HashMap var val, x) (HashMap var val, y)
runStoreT = coerce
{-# INLINE runStoreT #-}

-- | Execute a computation and only return the result value.
evalStoreT :: (Profunctor c) => StoreT var val c x y -> c (HashMap var val, x) y
evalStoreT f = rmap pi2 (runStoreT f)

-- | Execute a computation and only return the result store.
execStoreT :: (Profunctor c) => StoreT var val c x y -> c (HashMap var val, x) (HashMap var val)
execStoreT f = rmap pi1 (runStoreT f)

instance (Identifiable var, ArrowChoice c, Profunctor c) => ArrowStore var val (StoreT var val c) where
  type Join y (StoreT var val c) = ()
  read (StoreT f) (StoreT g) = StoreT $ proc (var,x) -> do
    s <- get -< ()
    case S.lookup var s of
      Just v -> f -< (v,x)
      Nothing -> g -< x
  write = StoreT $ modify $ arr (\((x,v),s) -> ((),S.insert x v s))

instance (ArrowApply c,Profunctor c) => ArrowApply (StoreT var val c) where app = StoreT ((\(StoreT f,x) -> (f,x)) ^>> app)

instance ArrowFix (Underlying (StoreT var val c) x y) => ArrowFix (StoreT var val c x y)
type instance Fix (StoreT var val c) x y = StoreT var val (Fix c (HashMap var val,x) (HashMap var val,y))
