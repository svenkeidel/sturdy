{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ValueSemantics.Unit where

import           Prelude hiding (Bool(..),Bounded(..),(.))

import           Syntax
import           SharedSemantics
import qualified SharedSemantics as Shared

import qualified Data.Abstract.Environment as E
import           Data.Abstract.Error (Error(..))
import           Data.Abstract.FreeCompletion (FreeCompletion)
import qualified Data.Abstract.FreeCompletion as FC
import           Data.Abstract.FreeCompletionComplete ()
import           Data.Abstract.Powerset (Pow)
import qualified Data.Abstract.Powerset as Pow
import qualified Data.Abstract.Store as S
import           Data.Abstract.Terminating

import           Data.Order
import           Data.Label
import           Data.Text (Text)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Environment
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Store
import           Control.Arrow.Try
import           Control.Arrow.Transformer.Abstract.Environment
import           Control.Arrow.Transformer.Abstract.Except
import           Control.Arrow.Transformer.Abstract.LeastFixPoint
import           Control.Arrow.Transformer.Abstract.UncertainStore
import           Control.Monad.State

-- Value semantics for the while language that does not approximate values at all.
type Addr = FreeCompletion (Pow Label)
type Env = E.Env Text Addr
type Store = UncertainStore Label Val
type Val = ()
newtype Interp c x y = Interp (Environment Text Addr (UncertainStoreArrow Label Val (Except String c)) x y)
type instance Fix x y (Interp c) = Interp (Fix (Store,(Env,x)) (Error String (Store,y)) c)

runInterp :: ArrowChoice c => Interp c x y -> c (Store,(Env,x)) (Error String (Store,y))
runInterp (Interp f) = runExcept (runUncertainStore (runEnvironment' f))

run :: [State Label Statement] -> Terminating (Error String Store)
run ss = fmap fst <$> runLeastFixPoint (runInterp (Shared.run :: Fix [Statement] () (Interp (~>)) [Statement] ())) ((S.empty,bottom),(E.empty, generate (sequence ss)))

instance ArrowChoice c => IsVal Val Addr (Interp c) where
  boolLit = arr (const ())
  and = arr (const ())
  or = arr (const ())
  not = arr (const ())
  numLit = arr (const ())
  randomNum = arr (const ())
  add = arr (const ())
  sub = arr (const ())
  mul = arr (const ())
  div = arr (const ())
  eq = arr (const ())
  lt = arr (const ())
  freshAddr = arr $ FC.Lower . Pow.singleton
  ref = arr (const ())
  getAddr = arr (const FC.Top)

instance (Complete (Interp c (x,y) z), ArrowChoice c)
  => Conditional Val x y z (Interp c) where
  if_ f1 f2 = proc (_,(x,y)) -> joined f1 f2 -< (x,y)

deriving instance ArrowChoice c => Category (Interp c)
deriving instance ArrowChoice c => Arrow (Interp c)
deriving instance ArrowChoice c => ArrowChoice (Interp c)
--deriving instance (ArrowChoice c, ArrowLoop c) => ArrowLoop (Interp c)
deriving instance ArrowChoice c => ArrowFail String (Interp c)
deriving instance (ArrowChoice c, ArrowFix (Store,(Env,x)) (Error String (Store,y)) c)
 => ArrowFix x y (Interp c)
deriving instance (Complete (c (Val,Label) (Error String Val)), ArrowChoice c)
 => ArrowStore Addr Val Label (Interp c)
deriving instance ArrowChoice c => ArrowEnv Text Addr Env (Interp c)
deriving instance (ArrowChoice c, Complete (c ((Store, (Env, Addr)), (Store, (Env, (Text, Label)))) (Error String (Store, Addr))))
  => ArrowTry (Text,Label) Addr Addr (Interp c)
deriving instance (PreOrd (c (Store,(Env,x)) (Error String (Store,y)))) => PreOrd (Interp c x y)
deriving instance (Complete (c (Store,(Env,x)) (Error String (Store,y)))) => Complete (Interp c x y)
deriving instance (UpperBounded (c (Store,(Env,x)) (Error String (Store,y)))) => UpperBounded (Interp c x y)

