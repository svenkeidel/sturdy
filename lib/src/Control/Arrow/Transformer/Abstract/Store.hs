{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Transformer.Abstract.Store where

import Control.Category
import Control.Arrow
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Fail
import Control.Arrow.Store
import Control.Arrow.Transformer.State

import Data.Abstract.Error
import Data.Abstract.Store (Store)
import qualified Data.Abstract.Store as S
import Data.Order
import Data.Identifiable

import Text.Printf

newtype StoreArrow var val c x y = StoreArrow (StateArrow (Store var val) c x y)

liftStore :: Arrow c => c x y -> StoreArrow var val c x y
liftStore f = StoreArrow (StateArrow (second f))

instance (Show var, Identifiable var, ArrowFail String c, ArrowChoice c, Complete (c ((Store var val,val),var) (Store var val,val)), LowerBounded (c () (Store var val,val))) =>
  ArrowStore var val (StoreArrow var val c) where
  lookup =
    StoreArrow $ StateArrow $ proc (s,var) -> case S.lookup var s of
      Success v -> joined returnA (proc var -> failA -< printf "could not find variable" (show var)) -< ((s,v),var)
      Fail _ -> failA -< printf "could not find variable" (show var)
      Bot -> bottom -< ()
  store = StoreArrow (StateArrow (arr (\(s,(x,v)) -> (S.insert x v s,()))))

instance ArrowState s c => ArrowState s (StoreArrow var val c) where
  getA = liftStore getA
  putA = liftStore putA

deriving instance Category c => Category (StoreArrow var val c)
deriving instance Arrow c => Arrow (StoreArrow var val c)
deriving instance ArrowChoice c => ArrowChoice (StoreArrow var val c)
deriving instance ArrowReader r c => ArrowReader r (StoreArrow var val c)
deriving instance ArrowFail e c => ArrowFail e (StoreArrow var val c)
deriving instance PreOrd (c (Store var val,x) (Store var val,y)) => PreOrd (StoreArrow var val c x y)
deriving instance Complete (c (Store var val,x) (Store var val,y)) => Complete (StoreArrow var val c x y)
deriving instance CoComplete (c (Store var val,x) (Store var val,y)) => CoComplete (StoreArrow var val c x y)
deriving instance UpperBounded (c (Store var val,x) (Store var val,y)) => UpperBounded (StoreArrow var val c x y)
deriving instance LowerBounded (c (Store var val,x) (Store var val,y)) => LowerBounded (StoreArrow var val c x y)
