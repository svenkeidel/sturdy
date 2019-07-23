{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Abstract.Store where

import Prelude hiding (Maybe(..))
import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Trans
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store
import Control.Arrow.Except
import Control.Arrow.Environment
import Control.Arrow.Transformer.State
import Control.Arrow.Utils
import Control.Category

import Control.Arrow.Order

import Data.Abstract.Maybe
import Data.Abstract.Map (Map)
import qualified Data.Abstract.Map as M

import Data.Order (Complete)
import Data.Identifiable
import Data.Hashable
import Data.Profunctor
import Data.Profunctor.Unsafe((.#))
import Data.Coerce

newtype StoreT var val c x y = StoreT (StateT (Map var val) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans,ArrowLift,
            ArrowReader r, ArrowFail e, ArrowExcept e, ArrowEnv var val env,
            ArrowConst r, ArrowLowerBounded, ArrowRun)

runStoreT :: StoreT var val c x y -> c (Map var val, x) (Map var val, y)
runStoreT = coerce
{-# INLINE runStoreT #-}

evalStoreT :: Profunctor c => StoreT var val c x y -> c (Map var val, x) y
evalStoreT f = rmap pi2 (runStoreT f)

execStoreT :: Profunctor c => StoreT var val c x y -> c (Map var val, x) (Map var val)
execStoreT f = rmap pi1 (runStoreT f)

instance (Identifiable var, ArrowChoice c, ArrowComplete c, Profunctor c, Complete val) => ArrowStore var val (StoreT var val c) where
  type Join (StoreT var val c) ((val,x),x) y = Complete y
  read (StoreT f) (StoreT g) = StoreT $ proc (var,x) -> do
    s <- get -< ()
    case M.lookup var s of
      Just val        -> f -< (val,x)
      Nothing         -> g -< x
      JustNothing val -> (f -< (val,x)) <⊔> (g -< x)
  write = StoreT $ modify $ arr $ \((var,val),st) -> ((),M.insert var val st)

instance ArrowState s c => ArrowState s (StoreT var val c) where
  get = lift' get
  put = lift' put

deriving instance (Eq var,Hashable var,Complete val,ArrowComplete c) => ArrowComplete (StoreT var val c)
instance (ArrowApply c, Profunctor c) => ArrowApply (StoreT var val c) where
  app = StoreT (app .# first coerce)

type instance Fix x y (StoreT var val c) = StoreT var val (Fix (Dom (StoreT var val) x y) (Cod (StoreT var val) x y) c)
deriving instance ArrowFix (Dom (StoreT var val) x y) (Cod (StoreT var val) x y) c => ArrowFix x y (StoreT var val c)
