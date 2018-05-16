{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Control.Arrow.Transformer.Abstract.LiveVariables where

import           Prelude hiding (id,(.),read)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Lift
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Store
import           Control.Arrow.Writer
import           Control.Arrow.Transformer.Effect

import           Data.HashSet (HashSet)
import qualified Data.HashSet as H
import           Data.Identifiable
import           Data.Order
import           Data.Abstract.Widening

import           GHC.Exts
import           GHC.Generics
import           Text.Printf

data LiveVars v = LiveVars (HashSet v) | Top deriving (Eq,Generic)

empty :: LiveVars v
empty = LiveVars H.empty

instance Show v => Show (LiveVars v) where
  show (LiveVars vs) = show (H.toList vs)
  show Top = "⊤"

instance Identifiable v => PreOrd (LiveVars v) where
  _ ⊑ Top = True
  LiveVars xs ⊑ LiveVars ys = all (\x -> H.member x ys) xs
  _ ⊑ _ = False

instance Identifiable v => Complete (LiveVars v) where
  Top ⊔ _ = Top
  _ ⊔ Top = Top
  LiveVars xs ⊔ LiveVars ys = LiveVars (H.union xs ys)

instance Identifiable v => Widening (LiveVars v)

instance Identifiable v => UpperBounded (LiveVars v) where
  top = Top

instance Identifiable v => IsList (LiveVars v) where
  type Item (LiveVars v) = v
  fromList = LiveVars . H.fromList
  toList (LiveVars vs) = H.toList vs
  toList Top = error "toList ⊤"

live :: Identifiable v => v -> LiveVars v -> LiveVars v
live x (LiveVars vars) = LiveVars (H.insert x vars)
live _ Top = Top

dead :: Identifiable v => v -> LiveVars v -> (LiveVars v)
dead x (LiveVars vars) = LiveVars (H.delete x vars)
dead _ Top = Top

newtype LiveVariables v c x y = LiveVariables (Backward (LiveVars v) c x y)

runLiveVariables :: LiveVariables v c x y -> c (LiveVars v,x) y
runLiveVariables (LiveVariables f) = runWriter (runState f)

instance (Identifiable var, ArrowStore var val lab c) => ArrowStore var val lab (LiveVariables var c) where
  read = LiveVariables $ proc (x,l) -> do
    effect live -< x
    read -< (x,l)
  write = LiveVariables $ proc (x,v,l) -> do
    modifyA' dead -< x
    write -< (x,v,l)

type instance Fix x y (LiveVariables v c) = LiveVariables v (Fix x (LiveVarsTrans v,y) c)
instance (ArrowFix (LiveVarsTrans v,x) (LiveVarsTrans v,(LiveVarsTrans v,y)) c) => ArrowFix x y (LiveVariables v c) where
  fixA f = LiveVariables (State (Writer (fixA (runLiveVariables . f . LiveVariables . State . Writer))))

instance ArrowLift (LiveVariables v) where
  lift f = LiveVariables (lift (lift f))

instance (ArrowApply c) => ArrowApply (LiveVariables v c) where
  app = LiveVariables ((\(LiveVariables f,x) -> (f,x)) ^>> app)

deriving instance (Arrow c) => Category (LiveVariables v c)
deriving instance (Arrow c) => Arrow (LiveVariables v c)
deriving instance (ArrowChoice c) => ArrowChoice (LiveVariables v c)
deriving instance (ArrowReader r c) => ArrowReader r (LiveVariables v c)
deriving instance (ArrowFail e c) => ArrowFail e (LiveVariables v c)
-- deriving instance (ArrowState s c) => ArrowState s (LiveVariables v c)

deriving instance PreOrd (c (LiveVarsTrans v,x) (LiveVarsTrans v,(LiveVarsTrans v,y))) => PreOrd (LiveVariables v c x y)
deriving instance LowerBounded (c (LiveVarsTrans v,x) (LiveVarsTrans v,(LiveVarsTrans v,y))) => LowerBounded (LiveVariables v c x y)
deriving instance Complete (c (LiveVarsTrans v,x) (LiveVarsTrans v,(LiveVarsTrans v,y))) => Complete (LiveVariables v c x y)
deriving instance CoComplete (c (LiveVarsTrans v,x) (LiveVarsTrans v,(LiveVarsTrans v,y))) => CoComplete (LiveVariables v c x y)
deriving instance UpperBounded (c (LiveVarsTrans v,x) (LiveVarsTrans v,(LiveVarsTrans v,y))) => UpperBounded (LiveVariables v c x y)

