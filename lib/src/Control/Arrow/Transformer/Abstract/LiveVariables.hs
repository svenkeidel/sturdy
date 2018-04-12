{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Abstract.LiveVariables where

import Prelude hiding (id,(.),read)

import Control.Category
import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Lift
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store
import Control.Arrow.Transformer.BackwardState

import Data.HashSet (HashSet)
import Data.Hashable
import qualified Data.HashSet as H
import Data.Identifiable
import Data.Order
import Data.Abstract.Widening
import GHC.Exts

newtype LiveVars v = LiveVars (HashSet v) deriving (Eq,Hashable)

instance Show v => Show (LiveVars v) where
  show (LiveVars vs) = show (H.toList vs)

instance Identifiable v => PreOrd (LiveVars v) where
  LiveVars xs ⊑ LiveVars ys = all (\x -> H.member x ys) xs

instance Identifiable v => Complete (LiveVars v) where
  LiveVars xs ⊔ LiveVars ys = LiveVars (H.union xs ys)

instance Identifiable v => Widening (LiveVars v)

instance Identifiable v => IsList (LiveVars v) where
  type Item (LiveVars v) = v
  fromList = LiveVars . H.fromList
  toList (LiveVars vs) = H.toList vs

empty :: LiveVars v
empty = LiveVars H.empty

live :: Identifiable v => v -> LiveVars v -> LiveVars v
live x (LiveVars vars) = LiveVars (H.insert x vars)

dead :: Identifiable v => v -> LiveVars v -> LiveVars v
dead x (LiveVars vars) = LiveVars (H.delete x vars)

newtype LiveVariables v c x y = LiveVariables (State (LiveVars v) c x y)

runLiveVariables :: LiveVariables v c x y -> c (LiveVars v,x) (LiveVars v,y)
runLiveVariables (LiveVariables (State f)) = f

instance (Identifiable var, ArrowLoop c, ArrowStore var val c) => ArrowStore var val (LiveVariables var c) where
  read = LiveVariables $ State $ proc (vars,x) -> do
    v <- read -< x
    returnA -< (live x vars,v)
  write = LiveVariables $ State $ proc (vars,(x,v)) -> do
    () <- write -< (x,v)
    returnA -< (dead x vars,())

instance ArrowLift (LiveVariables r) where
  lift f = LiveVariables (lift f)

instance (ArrowLoop c, ArrowApply c) => ArrowApply (LiveVariables v c) where
  app = LiveVariables (State (arr (\(p,(LiveVariables (State f),b)) -> (f,(p,b))) >>> app))

instance (ArrowLoop c, ArrowState s c) => ArrowState s (LiveVariables v c) where
  getA = lift getA
  putA = lift putA

type instance Fix x y (LiveVariables v c) = LiveVariables v (Fix (LiveVars v,x) (LiveVars v,y) c)
instance (ArrowLoop c, ArrowFix (LiveVars v,x) (LiveVars v,y) c) => ArrowFix x y (LiveVariables v c) where
  fixA f = LiveVariables (State (fixA (runLiveVariables . f . LiveVariables . State)))

deriving instance ArrowLoop c => Category (LiveVariables v c) 
deriving instance ArrowLoop c => Arrow (LiveVariables v c) 
deriving instance (ArrowLoop c, ArrowChoice c) => ArrowChoice (LiveVariables v c) 
deriving instance (ArrowLoop c, ArrowReader r c) => ArrowReader r (LiveVariables v c)
deriving instance (ArrowLoop c, ArrowFail e c) => ArrowFail e (LiveVariables v c)
