{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Concrete.Store where

import Prelude hiding ((.))

import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Lift
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store
import Control.Arrow.Transformer.State
import Control.Arrow.Try
import Control.Arrow.Utils
import Control.Category

import Data.Concrete.Error
import Data.Concrete.Store (Store)
import qualified Data.Concrete.Store as S
import Data.Identifiable

import Text.Printf

newtype StoreArrow var val c x y = StoreArrow (State (Store var val) c x y)

runStore :: StoreArrow var val c x y -> c (Store var val, x) (Store var val, y)
runStore (StoreArrow (State f)) = f

evalStore :: Arrow c => StoreArrow var val c x y -> c (Store var val, x) y
evalStore f = runStore f >>> pi2

execStore :: Arrow c => StoreArrow var val c x y -> c (Store var val, x) (Store var val)
execStore f = runStore f >>> pi1

instance ArrowLift (StoreArrow var val) where
  lift f = StoreArrow (lift f)

instance (Show var, Identifiable var, ArrowFail String c, ArrowChoice c) =>
  ArrowStore var val lab (StoreArrow var val c) where
  read =
    StoreArrow $ State $ proc (s,(var,_)) -> case S.lookup var s of
      Success v -> returnA -< (s,v)
      Fail _ -> failA -< printf "Variable %s not bound" (show var)
  write = StoreArrow (State (arr (\(s,(x,v,_)) -> (S.insert x v s,()))))

instance ArrowState s c => ArrowState s (StoreArrow var val c) where
  getA = lift getA
  putA = lift putA

instance (ArrowTry (Store var val,x) (Store var val,y) (Store var val,z) c) => ArrowTry x y z (StoreArrow var val c) where
  tryA (StoreArrow f) (StoreArrow g) (StoreArrow h) = StoreArrow (tryA f g h)

deriving instance Category c => Category (StoreArrow var val c)
deriving instance Arrow c => Arrow (StoreArrow var val c)
deriving instance ArrowChoice c => ArrowChoice (StoreArrow var val c)
deriving instance ArrowReader r c => ArrowReader r (StoreArrow var val c)
deriving instance ArrowFail e c => ArrowFail e (StoreArrow var val c)

type instance Fix x y (StoreArrow var val c) = StoreArrow var val (Fix (Store var val,x) (Store var val,y) c)
deriving instance ArrowFix (Store var val, x) (Store var val, y) c => ArrowFix x y (StoreArrow var val c)
