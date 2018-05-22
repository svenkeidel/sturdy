{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Abstract.UncertainStore where

import Prelude hiding (id)

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

import Data.Abstract.Error
import Data.Abstract.FreeCompletion
import qualified Data.Abstract.Store as St
import Data.Abstract.Powerset (Pow)
import qualified Data.Abstract.Store as S
import Data.Foldable (toList)
import Data.Hashable
import Data.Order
import Data.Identifiable

import Text.Printf

type UncertainStore var val = (St.Store var val, val)

newtype UncertainStoreArrow var val c x y = UncertainStoreArrow (State (UncertainStore var val) c x y)

runUncertainStore :: UncertainStoreArrow var val c x y -> c (UncertainStore var val, x) (UncertainStore var val, y)
runUncertainStore (UncertainStoreArrow (State f)) = f

evalStore :: Arrow c => UncertainStoreArrow var val c x y -> c (UncertainStore var val, x) y
evalStore f = runUncertainStore f >>> pi2

execStore :: Arrow c => UncertainStoreArrow var val c x y -> c (UncertainStore var val, x) (UncertainStore var val)
execStore f = runUncertainStore f >>> pi1

instance (Show var, Hashable var, Identifiable var, ArrowFail String c, ArrowChoice c, Complete val, LowerBounded val, UpperBounded val, Complete (c (val,var) val)) =>
  ArrowStore (FreeCompletion (Pow var)) val lab (UncertainStoreArrow var val c) where
  read = UncertainStoreArrow $ State $ proc ((s,topvals),(a,_)) -> case a of
     Top -> returnA -< ((s,topvals),topvals) -- TODO: least upper bound of codomain
     Lower vars -> do
      v <- arr lub <<< mapA' readVar -< (toList vars,s)
      returnA -< ((s,topvals),v ⊔ topvals)
    where
      readVar = proc (var,s) -> case S.lookup var s of
                    Success v -> joined returnA (proc var -> failA -< printf "Variable %s maybe not bound" (show var)) -< (v,var)
                    Fail _ -> failA -< printf "Variable %s not bound" (show var)
  write = UncertainStoreArrow $ State $ proc ((s,topvals),(a,v,_)) -> case a of
    Top -> returnA -< ((s,topvals ⊔ v),())
    Lower vars -> do
      s' <- foldA' writeVar -< (toList vars `zip` repeat v, s)
      returnA -< ((s',topvals),())
    where
      writeVar = arr $ \((var,v),s) -> S.insertWith (⊔) var v s

instance ArrowState s c => ArrowState s (UncertainStoreArrow var val c) where
  getA = lift getA
  putA = lift putA

instance (ArrowTry (UncertainStore var val,x) (UncertainStore var val,y) (UncertainStore var val,z) c) => ArrowTry x y z (UncertainStoreArrow var val c) where
  tryA (UncertainStoreArrow f) (UncertainStoreArrow g) (UncertainStoreArrow h) = UncertainStoreArrow (tryA f g h)

deriving instance Category c => Category (UncertainStoreArrow var val c)
deriving instance Arrow c => Arrow (UncertainStoreArrow var val c)
deriving instance ArrowChoice c => ArrowChoice (UncertainStoreArrow var val c)
deriving instance ArrowLift (UncertainStoreArrow var val)
deriving instance ArrowReader r c => ArrowReader r (UncertainStoreArrow var val c)
deriving instance ArrowFail e c => ArrowFail e (UncertainStoreArrow var val c)
deriving instance ArrowLoop c => ArrowLoop (UncertainStoreArrow var val c)
instance ArrowApply c => ArrowApply (UncertainStoreArrow var val c) where app = UncertainStoreArrow $ (\(UncertainStoreArrow f,x) -> (f,x)) ^>> app
type instance Fix x y (UncertainStoreArrow var val c) = UncertainStoreArrow var val (Fix (UncertainStore var val,x) (UncertainStore var val,y) c)
deriving instance ArrowFix (UncertainStore var val, x) (UncertainStore var val, y) c => ArrowFix x y (UncertainStoreArrow var val c)
deriving instance PreOrd (c (UncertainStore var val,x) (UncertainStore var val,y)) => PreOrd (UncertainStoreArrow var val c x y)
deriving instance Complete (c (UncertainStore var val,x) (UncertainStore var val,y)) => Complete (UncertainStoreArrow var val c x y)
deriving instance CoComplete (c (UncertainStore var val,x) (UncertainStore var val,y)) => CoComplete (UncertainStoreArrow var val c x y)
deriving instance UpperBounded (c (UncertainStore var val,x) (UncertainStore var val,y)) => UpperBounded (UncertainStoreArrow var val c x y)
deriving instance LowerBounded (c (UncertainStore var val,x) (UncertainStore var val,y)) => LowerBounded (UncertainStoreArrow var val c x y)
