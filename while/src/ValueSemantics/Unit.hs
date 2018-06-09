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

import           Prelude hiding (Bool(..),Bounded(..),read)

import           Syntax
import           SharedSemantics
import qualified SharedSemantics as Shared

import           Data.Abstract.PropagateError (Error(..))
import           Data.Abstract.Store (Store)
import qualified Data.Abstract.Store as S
import           Data.Abstract.Environment (Env)
import qualified Data.Abstract.Environment as E
import           Data.Abstract.Terminating

import           Data.Order
import           Data.Label
import           Data.Text (Text)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Alloc
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Store
import           Control.Arrow.Environment
import           Control.Arrow.Transformer.Abstract.PropagateExcept
import           Control.Arrow.Transformer.Abstract.LeastFixPoint
import           Control.Arrow.Transformer.Abstract.Store
import           Control.Arrow.Transformer.Abstract.Environment

-- Value semantics for the while language that does not approximate values at all.
type Addr = Label
type Val = ()
newtype Interp c x y = Interp (Environment Text Addr (StoreArrow Addr Val (Except String c)) x y)

runInterp :: ArrowChoice c => Interp c x y -> c (Store Addr Val,(Env Text Addr,x)) (Error String (Store Addr Val,y))
runInterp (Interp f) = runExcept (runStore (runEnvironment f))

run :: [(Text,Addr)] -> [LStatement] -> Terminating (Error String (Store Addr Val))
run env ss =
  fmap fst <$>
    runLeastFixPoint
      (runInterp
        (Shared.run :: Fix [Statement] () (Interp (~>)) [Statement] ()))
      (S.empty,(E.fromList env,generate <$> ss))

instance ArrowChoice c => ArrowAlloc (Text,Val,Label) Addr (Interp c) where
  alloc = arr $ \(_,_,l) -> l

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
deriving instance ArrowChoice c => ArrowFail String (Interp c)
deriving instance (ArrowChoice c) => ArrowEnv Text Addr (Env Text Addr) (Interp c)
deriving instance (PreOrd (c (Store Addr Val,(Env Text Addr,x)) (Error String (Store Addr Val,y)))) => PreOrd (Interp c x y)
deriving instance (Complete (c (Store Addr Val,(Env Text Addr,x)) (Error String (Store Addr Val,y)))) => Complete (Interp c x y)
deriving instance (UpperBounded (c (Store Addr Val,(Env Text Addr,x)) (Error String (Store Addr Val,y)))) => UpperBounded (Interp c x y)

instance (ArrowChoice c,Complete (c (Store Label Val, ((Val, (Env Text Label, x)), (Env Text Label, x))) (Error [Char] (Store Label Val, y)))) => ArrowRead (Addr,Label) Val x y (Interp c) where
  read (Interp f) (Interp g) = Interp $ proc ((addr,_),x) -> read f g -< (addr,x)
                               
instance ArrowChoice c => ArrowWrite (Addr,Label) Val (Interp c) where
  write = Interp $ proc ((addr,_),val) -> write -< (addr,val)

type instance Fix x y (Interp c) = Interp (Fix (Store Addr Val,(Env Text Addr,x)) (Error String (Store Addr Val,y)) c)
deriving instance (ArrowChoice c, ArrowFix (Store Addr Val,(Env Text Addr,x)) (Error String (Store Addr Val,y)) c) => ArrowFix x y (Interp c)
