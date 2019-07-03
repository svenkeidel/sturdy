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

import           Prelude hiding (Bool(..),Bounded(..))

import           Syntax
import           GenericInterpreter
import qualified GenericInterpreter as Generic

import           Data.Abstract.Error (Error(..))
import qualified Data.Abstract.Map as M
import qualified Data.Abstract.StrongMap as SM
import           Data.Abstract.Terminating
import           Data.Abstract.FreeCompletion(FreeCompletion)
import           Data.Abstract.DiscretePowerset(Pow)
import qualified Data.Abstract.IterationStrategy as S
import qualified Data.Abstract.StackWidening as SW
import qualified Data.Abstract.Widening as W

import           Data.Order
import           Data.Label
import           Data.Text (Text)
import           Data.Profunctor
import qualified Data.Lens as L

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Alloc
import           Control.Arrow.Fix
import           Control.Arrow.Fail
import           Control.Arrow.Environment
import           Control.Arrow.Store
import           Control.Arrow.Random
import           Control.Arrow.Abstract.Join

import           Control.Arrow.Transformer.Abstract.Environment
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Store
import           Control.Arrow.Transformer.Abstract.Terminating

-- Value semantics for the while language that does not approximate values at all.
type Addr = FreeCompletion Label
type Val = ()

run :: [(Text,Addr)] -> [LStatement] -> Terminating (Error (Pow String) (M.Map Addr Val))
run env ss =
  fmap fst <$>
    runFixT iterationStrategy
      (runTerminatingT
         (runErrorT
           (runStoreT
             (runEnvT
               (runUnitT
                 (Generic.run ::
                   Fix [Statement] ()
                     (UnitT
                       (EnvT Text Addr
                         (StoreT Addr Val
                           (ErrorT (Pow String)
                             (TerminatingT
                               (FixT () () _)))))) [Statement] ()))))))
      (M.empty,(SM.fromList env,generate <$> ss))
  where
    iterationStrategy = S.filter (L.second (L.second whileLoops))
                      $ S.chaotic SW.finite W.finite

newtype UnitT c x y = UnitT { runUnitT :: c x y }
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowFail e,ArrowEnv var val env,ArrowStore var val,ArrowJoin,PreOrd,Complete)
type instance Fix x y (UnitT c) = UnitT (Fix x y c)
deriving instance ArrowFix x y c => ArrowFix x y (UnitT c)

instance (ArrowChoice c,Profunctor c) => ArrowAlloc (Text,Val,Label) Addr (UnitT c) where
  alloc = arr $ \(_,_,l) -> return l

instance (ArrowChoice c, ArrowJoin c) => IsVal Val (UnitT c) where
  type Join (UnitT c) x y = Complete y
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

instance (ArrowChoice c,Profunctor c) => ArrowRand Val (UnitT c) where
  random = arr (const ())
