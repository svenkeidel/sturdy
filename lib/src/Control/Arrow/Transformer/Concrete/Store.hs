{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Transformer.Concrete.Store where

import Control.Category
import Control.Arrow
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Fail
import Control.Arrow.Store
import Control.Arrow.Lift
import Control.Arrow.Transformer.State

import Data.Concrete.Error
import Data.Concrete.Store (Store)
import qualified Data.Concrete.Store as S
import Data.Order
import Data.Identifiable

import Text.Printf

newtype StoreArrow var val c x y = StoreArrow (State (Store var val) c x y)

instance ArrowLift (StoreArrow var val) where
  lift f = StoreArrow (lift f)

instance (Show var, Identifiable var, ArrowFail String c, ArrowChoice c, Complete (c ((Store var val,val),var) (Store var val,val))) =>
  ArrowStore var val (StoreArrow var val c) where
  lookup =
    StoreArrow $ State $ proc (s,var) -> case S.lookup var s of
      Success v -> joined returnA (proc var -> failA -< printf "could not find variable" (show var)) -< ((s,v),var)
      Fail _ -> failA -< printf "could not find variable" (show var)
  store = StoreArrow (State (arr (\(s,(x,v)) -> (S.insert x v s,()))))

instance ArrowState s c => ArrowState s (StoreArrow var val c) where
  getA = lift getA
  putA = lift putA

deriving instance Category c => Category (StoreArrow var val c)
deriving instance Arrow c => Arrow (StoreArrow var val c)
deriving instance ArrowChoice c => ArrowChoice (StoreArrow var val c)
deriving instance ArrowReader r c => ArrowReader r (StoreArrow var val c)
deriving instance ArrowFail e c => ArrowFail e (StoreArrow var val c)
