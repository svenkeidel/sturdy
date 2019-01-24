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

import Control.Arrow.Abstract.Join

import Data.Abstract.Maybe
import Data.Abstract.Map (Map)
import qualified Data.Abstract.Map as M
import Data.Order
import Data.Identifiable
import Data.Hashable

newtype StoreT var val c x y = StoreT (StateT (Map var val) c x y)

runStoreT :: StoreT var val c x y -> c (Map var val, x) (Map var val, y)
runStoreT (StoreT (StateT f)) = f

evalStoreT :: Arrow c => StoreT var val c x y -> c (Map var val, x) y
evalStoreT f = runStoreT f >>> pi2

execStoreT :: Arrow c => StoreT var val c x y -> c (Map var val, x) (Map var val)
execStoreT f = runStoreT f >>> pi1

instance (Identifiable var, ArrowChoice c) => ArrowStore var val (StoreT var val c) where
  type Join (StoreT var val c) ((val,x),x) y = Complete (c (Map var val, ((val, x), x)) (Map var val, y))
  read (StoreT f) (StoreT g) = StoreT $ proc (var,x) -> do
    s <- get -< ()
    case M.lookup var s of
      Just val        -> f          -< (val,x)
      JustNothing val -> joined f g -< ((val,x),x)
      Nothing         -> g          -< x
  write = StoreT $ modify $ arr $ \((var,val),st) -> M.insert var val st

instance ArrowState s c => ArrowState s (StoreT var val c) where
  get = lift' get
  put = lift' put

deriving instance (Eq var,Hashable var,Complete val,ArrowJoin c) => ArrowJoin (StoreT var val c)
deriving instance Arrow c => Category (StoreT var val c)
deriving instance Arrow c => Arrow (StoreT var val c)
deriving instance ArrowChoice c => ArrowChoice (StoreT var val c)
deriving instance ArrowTrans (StoreT var val)
deriving instance ArrowLift (StoreT var val)
deriving instance ArrowReader r c => ArrowReader r (StoreT var val c)
deriving instance ArrowFail e c => ArrowFail e (StoreT var val c)
deriving instance ArrowExcept e c => ArrowExcept e (StoreT var val c)
deriving instance ArrowEnv x y env c => ArrowEnv x y env (StoreT var val c)
deriving instance ArrowConst x c => ArrowConst x (StoreT var val c)
instance ArrowApply c => ArrowApply (StoreT var val c) where app = StoreT $ (\(StoreT f,x) -> (f,x)) ^>> app

type instance Fix x y (StoreT var val c) = StoreT var val (Fix (Dom (StoreT var val) x y) (Cod (StoreT var val) x y) c)
deriving instance ArrowFix (Dom (StoreT var val) x y) (Cod (StoreT var val) x y) c => ArrowFix x y (StoreT var val c)

deriving instance PreOrd (c (Map var val,x) (Map var val,y)) => PreOrd (StoreT var val c x y)
deriving instance Complete (c (Map var val,x) (Map var val,y)) => Complete (StoreT var val c x y)
deriving instance CoComplete (c (Map var val,x) (Map var val,y)) => CoComplete (StoreT var val c x y)
deriving instance UpperBounded (c (Map var val,x) (Map var val,y)) => UpperBounded (StoreT var val c x y)
deriving instance LowerBounded (c (Map var val,x) (Map var val,y)) => LowerBounded (StoreT var val c x y)
