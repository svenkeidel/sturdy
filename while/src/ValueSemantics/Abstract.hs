{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ValueSemantics.Abstract where

import           Prelude hiding (read)

import           Data.Abstract.PropagateError (Error(..))
import           Data.Abstract.Store (Store)
import           Data.Abstract.Environment (Env)
import           Data.Order
import           Data.Label
import           Data.Text (Text)
import           Data.Identifiable

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Store
import           Control.Arrow.Environment
import           Control.Arrow.Const
import           Control.Arrow.Transformer.Abstract.PropagateExcept
import           Control.Arrow.Transformer.Abstract.Store
import           Control.Arrow.Transformer.Abstract.Environment

newtype Interp addr val c x y = Interp (Environment Text addr (StoreArrow addr val (Except String c)) x y)

runInterp :: ArrowChoice c => Interp addr val c x y -> c (Store addr val,(Env Text addr,x)) (Error String (Store addr val,y))
runInterp (Interp f) = runExcept (runStore (runEnvironment f))
                       
deriving instance (Identifiable addr, ArrowChoice c,Complete (c (Store addr val, ((val, (Env Text addr, x)), (Env Text addr, x))) (Error [Char] (Store addr val, y)))) => ArrowRead addr val x y (Interp addr val c)
deriving instance (Identifiable addr, ArrowChoice c) => ArrowWrite addr val (Interp addr val c)

instance (Identifiable addr, ArrowChoice c,Complete (c (Store addr val, ((val, (Env Text addr, x)), (Env Text addr, x))) (Error [Char] (Store addr val, y)))) => ArrowRead (addr,Label) val x y (Interp addr val c) where
  read (Interp f) (Interp g) = Interp $ proc ((addr,_),x) -> read f g -< (addr,x)
instance (Identifiable addr, ArrowChoice c) => ArrowWrite (addr,Label) val (Interp addr val c) where
  write = Interp $ proc ((addr,_),val) -> write -< (addr,val)

deriving instance ArrowChoice c => Category (Interp addr val c)
deriving instance ArrowChoice c => Arrow (Interp addr val c)
deriving instance ArrowChoice c => ArrowChoice (Interp addr val c)
deriving instance ArrowChoice c => ArrowFail String (Interp addr val c)
deriving instance (ArrowChoice c, ArrowConst x c) => ArrowConst x (Interp addr val c)
type instance Fix x y (Interp addr val c) = Interp addr val (Fix (Store addr val,(Env Text addr,x)) (Error String (Store addr val,y)) c)
deriving instance (ArrowChoice c, ArrowFix (Store addr val,(Env Text addr,x)) (Error String (Store addr val,y)) c) => ArrowFix x y (Interp addr val c)
deriving instance ArrowChoice c => ArrowEnv Text addr (Env Text addr) (Interp addr val c)
deriving instance (PreOrd (c (Store addr val,(Env Text addr,x)) (Error String (Store addr val,y)))) => PreOrd (Interp addr val c x y)
deriving instance (Complete (c (Store addr val,(Env Text addr,x)) (Error String (Store addr val,y)))) => Complete (Interp addr val c x y)
deriving instance (UpperBounded (c (Store addr val,(Env Text addr,x)) (Error String (Store addr val,y)))) => UpperBounded (Interp addr val c x y)
