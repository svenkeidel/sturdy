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

import Data.Identifiable
import Data.Profunctor
import Data.Profunctor.Unsafe((.#))
import Data.Coerce

newtype StoreT var val c x y = StoreT (StateT (Map var val) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans,ArrowLift,
            ArrowConst r, ArrowReader r,
            ArrowEnv var' val', ArrowClosure var' val' env,
            ArrowFail e, ArrowExcept e,
            ArrowLowerBounded, ArrowRun)

runStoreT :: StoreT var val c x y -> c (Map var val, x) (Map var val, y)
runStoreT = coerce
{-# INLINE runStoreT #-}

evalStoreT :: Profunctor c => StoreT var val c x y -> c (Map var val, x) y
evalStoreT f = rmap pi2 (runStoreT f)

execStoreT :: Profunctor c => StoreT var val c x y -> c (Map var val, x) (Map var val)
execStoreT f = rmap pi1 (runStoreT f)

instance (Identifiable var, ArrowChoice c, Profunctor c) => ArrowStore var val (StoreT var val c) where
  type Join y (StoreT var val c) = ArrowComplete (Map var val,y) c
  read (StoreT f) (StoreT g) = StoreT $ proc (var,x) -> do
    s <- get -< ()
    case M.lookup var s of
      Just val        -> f -< (val,x)
      Nothing         -> g -< x
      JustNothing val -> (f -< (val,x)) <⊔> (g -< x)
  write = StoreT $ modify $ arr $ \((var,val),st) -> ((),M.insert var val st)
  {-# INLINE read #-}
  {-# INLINE write #-}

instance ArrowState s c => ArrowState s (StoreT var val c) where
  get = lift' get
  put = lift' put
  {-# INLINE get #-}
  {-# INLINE put #-}

deriving instance (ArrowComplete (Map var val,y) c) => ArrowComplete y (StoreT var val c)
instance (ArrowApply c, Profunctor c) => ArrowApply (StoreT var val c) where
  app = StoreT (app .# first coerce)

type instance Fix (StoreT var val c) x y  = StoreT var val (Fix c (Map var val,x) (Map var val,y))
deriving instance ArrowFix (Underlying (StoreT var val c) x y) => ArrowFix (StoreT var val c x y)
