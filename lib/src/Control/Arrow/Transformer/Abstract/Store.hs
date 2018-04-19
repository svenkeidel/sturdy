{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Abstract.Store where

import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Lift
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store
import Control.Arrow.Transformer.State
import Control.Arrow.Utils
import Control.Category

import Data.Abstract.Error
import Data.Abstract.Store (Store)
import qualified Data.Abstract.Store as S
import Data.Order
import Data.Identifiable

import Text.Printf

newtype StoreArrow var val c x y = StoreArrow (State (Store var val) c x y)

runStore :: StoreArrow var val c x y -> c (Store var val, x) (Store var val, y)
runStore (StoreArrow (State f)) = f

evalStore :: Arrow c => StoreArrow var val c x y -> c (Store var val, x) y
evalStore f = runStore f >>> pi2

execStore :: Arrow c => StoreArrow var val c x y -> c (Store var val, x) (Store var val)
execStore f = runStore f >>> pi1

instance (Show var, Identifiable var, ArrowFail String c, ArrowChoice c, Complete (c ((Store var val,val),var) (Store var val,val))) =>
  ArrowStore var val lab (StoreArrow var val c) where
  read =
    StoreArrow $ State $ proc (s,(var,_)) -> case S.lookup var s of
      Success v -> joined returnA (proc var -> failA -< printf "Variable %s not bound" (show var)) -< ((s,v),var)
      Fail _ -> failA -< printf "Variable %s not bound" (show var)
  write = StoreArrow (State (arr (\(s,(x,v,_)) -> (S.insert x v s,()))))

instance ArrowState s c => ArrowState s (StoreArrow var val c) where
  getA = lift getA
  putA = lift putA

deriving instance Category c => Category (StoreArrow var val c)
deriving instance Arrow c => Arrow (StoreArrow var val c)
deriving instance ArrowChoice c => ArrowChoice (StoreArrow var val c)
deriving instance ArrowLift (StoreArrow var val)
deriving instance ArrowReader r c => ArrowReader r (StoreArrow var val c)
deriving instance ArrowFail e c => ArrowFail e (StoreArrow var val c)
deriving instance ArrowLoop c => ArrowLoop (StoreArrow var val c)
instance ArrowApply c => ArrowApply (StoreArrow var val c) where app = StoreArrow $ (\(StoreArrow f,x) -> (f,x)) ^>> app
type instance Fix x y (StoreArrow var val c) = StoreArrow var val (Fix (Store var val,x) (Store var val,y) c)
deriving instance ArrowFix (Store var val, x) (Store var val, y) c => ArrowFix x y (StoreArrow var val c)
deriving instance PreOrd (c (Store var val,x) (Store var val,y)) => PreOrd (StoreArrow var val c x y)
deriving instance Complete (c (Store var val,x) (Store var val,y)) => Complete (StoreArrow var val c x y)
deriving instance CoComplete (c (Store var val,x) (Store var val,y)) => CoComplete (StoreArrow var val c x y)
deriving instance UpperBounded (c (Store var val,x) (Store var val,y)) => UpperBounded (StoreArrow var val c x y)
deriving instance LowerBounded (c (Store var val,x) (Store var val,y)) => LowerBounded (StoreArrow var val c x y)
