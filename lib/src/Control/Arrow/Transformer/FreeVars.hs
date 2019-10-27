{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.FreeVars where

import Prelude hiding ((.),read,Maybe(..))

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Environment as Env
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Order
import           Control.Arrow.State
import           Control.Arrow.Store
import           Control.Arrow.Trans
import           Control.Arrow.Writer as Writer

import           Control.Arrow.Transformer.Writer

import           Data.Identifiable
import           Data.Profunctor
import           Data.Profunctor.Unsafe((.#))
import           Data.Coerce
import           Data.HashSet(HashSet)
import qualified Data.HashSet as H

newtype FreeVarsT var c x y = FreeVarsT (WriterT (HashSet var) c x y)
  deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowTrans, ArrowLift, ArrowLowerBounded,
            ArrowState s, ArrowFail e, ArrowExcept e, ArrowStore var' val', ArrowConst k)

runFreeVarsT :: Profunctor c => FreeVarsT var c x y -> c x y
runFreeVarsT (FreeVarsT f) = rmap snd (runWriterT f)
{-# INLINE runFreeVarsT #-}

instance (Identifiable var, ArrowRun c) => ArrowRun (FreeVarsT var c) where
  type Run (FreeVarsT var c) x y = Run c x y
  run = run . runFreeVarsT
  {-# INLINE run #-}

instance (Identifiable var, ArrowEnv var val c, Profunctor c) => ArrowEnv var val (FreeVarsT var c) where
  type Join y (FreeVarsT var c) = Env.Join (HashSet var,y) c
  lookup (FreeVarsT f) (FreeVarsT g) = FreeVarsT $ proc (var,x) -> do
    tell -< H.singleton var
    Env.lookup f g -< (var,x)
  extend (FreeVarsT f) = FreeVarsT $ proc (var,val,x) ->
    censor (\(var,_,_) -> H.delete var) (Env.extend f) -< (var,val,x)

instance (Identifiable var,ArrowApply c, Profunctor c) => ArrowApply (FreeVarsT var c) where
  app = FreeVarsT (app .# first coerce)

type instance Fix (FreeVarsT var c) x y = FreeVarsT var (Fix c x (HashSet var,y))
instance (ArrowFix (Underlying (FreeVarsT var c) x y)) => ArrowFix (FreeVarsT var c x y)
deriving instance (Identifiable var,ArrowComplete (HashSet var,y) c) => ArrowComplete y (FreeVarsT var c)
