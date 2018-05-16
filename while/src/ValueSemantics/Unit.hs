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

import           Prelude hiding (Bool(..),Bounded(..))

import           Syntax
import           SharedSemantics
import qualified SharedSemantics as Shared

import           Data.Abstract.Error (Error(..))
import           Data.Abstract.Store (Store)
import qualified Data.Abstract.Store as S
import           Data.Abstract.Terminating

import           Data.Order
import           Data.Label
import           Data.Text (Text)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Store
import           Control.Arrow.Transformer.Abstract.Except
import           Control.Arrow.Transformer.Abstract.LeastFixPoint
import           Control.Arrow.Transformer.Abstract.Store
import           Control.Monad.State

-- Value semantics for the while language that does not approximate values at all.
type Val = ()
newtype Interp c x y = Interp (StoreArrow Text Val (Except String c) x y)
type instance Fix x y (Interp c) = Interp (Fix (Store Text Val,x) (Error String (Store Text Val,y)) c)

runInterp :: Interp c x y -> c (Store Text Val,x) (Error String (Store Text Val,y))
runInterp (Interp f) = runExcept (runStore f)

run :: [State Label Statement] -> Terminating (Error String (Store Text Val))
run ss = fmap fst <$> runLeastFixPoint (runInterp (Shared.run :: Fix [Statement] () (Interp (~>)) [Statement] ())) (S.empty,generate (sequence ss))

instance ArrowChoice c => IsVal Val (Interp c) where
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

instance (Complete (Interp c (x,y) z), ArrowChoice c)
  => Conditional Val x y z (Interp c) where
  if_ f1 f2 = proc (_,(x,y)) -> joined f1 f2 -< (x,y)

deriving instance ArrowChoice c => Category (Interp c)
deriving instance ArrowChoice c => Arrow (Interp c)
deriving instance ArrowChoice c => ArrowChoice (Interp c)
deriving instance (ArrowChoice c, ArrowLoop c) => ArrowLoop (Interp c)
deriving instance ArrowChoice c => ArrowFail String (Interp c)
deriving instance (ArrowChoice c, ArrowFix (Store Text Val,x) (Error String (Store Text Val,y)) c) => ArrowFix x y (Interp c)
deriving instance (Complete (c ((Store Text Val,Val),Text) (Error String (Store Text Val,Val))), ArrowChoice c) => ArrowStore Text Val Label (Interp c)
deriving instance (PreOrd (c (Store Text Val,x) (Error String (Store Text Val,y)))) => PreOrd (Interp c x y)
deriving instance (Complete (c (Store Text Val,x) (Error String (Store Text Val,y)))) => Complete (Interp c x y)
deriving instance (UpperBounded (c (Store Text Val,x) (Error String (Store Text Val,y)))) => UpperBounded (Interp c x y)

