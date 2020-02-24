{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-partial-type-signatures #-}
module UnitSemantics where

import           Prelude hiding (Bool(..),Bounded(..),(.),filter)

import           Syntax
import           GenericInterpreter
import qualified GenericInterpreter as Generic

import           Data.Label
import           Data.Text (Text)
import           Data.Profunctor

import           Data.Abstract.Error (Error(..))
import           Data.Abstract.Except (Except(..))
import qualified Data.Abstract.Map as M
import qualified Data.Abstract.StrongMap as SM
import           Data.Abstract.Terminating
import           Data.Abstract.FreeCompletion(FreeCompletion)
import           Data.Abstract.DiscretePowerset(Pow)
import qualified Data.Abstract.Widening as W

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Arrow.Fix.Chaotic
import           Control.Arrow.Fail
import           Control.Arrow.Except
import           Control.Arrow.Environment
import           Control.Arrow.Store
import           Control.Arrow.Random
import           Control.Arrow.Order
import           Control.Arrow.Trans as Trans

import           Control.Arrow.Transformer.Abstract.Environment
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Except
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Component
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable
import           Control.Arrow.Transformer.Abstract.Fix.Stack
import           Control.Arrow.Transformer.Abstract.Store
import           Control.Arrow.Transformer.Abstract.Terminating

-- Value semantics for the while language that does not approximate values at all.
type Addr = FreeCompletion Label
type Env = SM.Map Text Addr
type Store = M.Map Addr Val
type Val = ()
type Exception = ()

run :: [(Text,Addr)] -> [LStatement] -> Terminating (Error (Pow String) (Except Exception (M.Map Addr Val)))
run env ss =
  fmap (fmap (fmap fst)) <$>
    snd $
    Trans.run
      (Generic.run ::
        Fix'
          (UnitT
            (EnvT Env
              (StoreT Store
                (ExceptT Exception
                  (ErrorT (Pow String)
                    (TerminatingT
                      (FixT _ _
                        (ComponentT _
                          (StackT Stack _
                            (CacheT Cache _ _
                               (->))))))))))) [Statement] ())
      iterationStrategy
      W.finite
      (M.empty,(SM.fromList env,generate <$> ss))
  where
    iterationStrategy = filter whileLoops
                        innermost

newtype UnitT c x y = UnitT { runUnitT :: c x y }
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowFail e,ArrowEnv var val,ArrowStore var val,ArrowExcept exc,ArrowComplete z)
type instance Fix (UnitT c) x y = UnitT (Fix c x y)
deriving instance ArrowFix (c x y) => ArrowFix (UnitT c x y)

instance (ArrowChoice c,Profunctor c) => ArrowAlloc Addr (UnitT c) where
  alloc = arr $ \(_,_,l) -> return l

instance (ArrowChoice c, ArrowComplete Val c) => IsVal Val (UnitT c) where
  type JoinVal y (UnitT c) = ArrowComplete y c
  boolLit = arr (const ())
  and = arr (const ())
  or = arr (const ())
  not = arr (const ())
  numLit = arr (const ())
  add = arr (const ())
  sub = arr (const ())
  mul = arr (const ())
  div = arr (const ())
  eq = arr (const ())
  lt = arr (const ())
  if_ f1 f2 = proc (_,(x,y)) -> (f1 -< x) <⊔> (f2 -< y)

instance (ArrowChoice c) => IsException Exception Val (UnitT c) where
  type JoinExc y (UnitT c) = ArrowComplete y c
  namedException = proc (_,_) -> returnA -< ()
  matchException f g = proc (_,(),x) -> (f -< ((),x)) <⊔> (g -< x)

instance ArrowRun c => ArrowRun (UnitT c) where
  type Run (UnitT c) x y = Run c x y
  run = Trans.run . runUnitT

instance (ArrowChoice c,Profunctor c) => ArrowRand Val (UnitT c) where
  random = arr (const ())
