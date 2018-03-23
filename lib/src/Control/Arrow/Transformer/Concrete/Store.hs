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
import Control.Arrow.Transformer.State

import Data.Concrete.Error
import Data.Concrete.Store (Store)
import qualified Data.Concrete.Store as S
import Data.Order
import Data.Identifiable

import Text.Printf

newtype StoreArrow var val c x y = StoreArrow (StateArrow (Store var val) c x y)

liftStore :: Arrow c => c x y -> StoreArrow var val c x y
liftStore f = StoreArrow (StateArrow (second f))

instance (Show var, Identifiable var, ArrowFail String c, ArrowChoice c, Complete (c ((Store var val,val),var) (Store var val,val))) =>
  ArrowStore var val (StoreArrow var val c) where
  lookup =
    StoreArrow $ StateArrow $ proc (s,var) -> case S.lookup var s of
      Success v -> joined returnA (proc var -> failA -< printf "could not find variable" (show var)) -< ((s,v),var)
      Fail _ -> failA -< printf "could not find variable" (show var)
  store = StoreArrow (StateArrow (arr (\(s,(x,v)) -> (S.insert x v s,()))))

instance ArrowState s c => ArrowState s (StoreArrow var val c) where
  getA = liftStore getA
  putA = liftStore putA

deriving instance Category c => Category (StoreArrow var val c)
deriving instance Arrow c => Arrow (StoreArrow var val c)
deriving instance ArrowChoice c => ArrowChoice (StoreArrow var val c)
deriving instance ArrowReader r c => ArrowReader r (StoreArrow var val c)
deriving instance ArrowFail e c => ArrowFail e (StoreArrow var val c)
