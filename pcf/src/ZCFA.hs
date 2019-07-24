{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-partial-type-signatures #-}
module ZCFA where

import           Prelude hiding (Bounded,fail,(.),exp)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Trans
import           Control.Arrow.Environment
import           Control.Arrow.Order hiding (bottom)
import           Control.Arrow.Transformer.Abstract.Environment
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Fix
import qualified Control.Arrow.Transformer.Abstract.Fix.IterationStrategy as S
import           Control.Arrow.Transformer.Abstract.Terminating
import           Control.Monad.State(State)

import           Data.Empty
import           Data.HashMap.Lazy(HashMap)
import           Data.Hashable
import           Data.Label
import           Data.Order
import           Data.Text (Text)
import           Data.Profunctor

import           Data.Abstract.Error (Error)
import qualified Data.Abstract.Widening as W
import           Data.Abstract.Terminating(Terminating)
import           Data.Abstract.DiscretePowerset (Pow,singleton)
    
import           GHC.Generics(Generic)
import           GHC.Exts(IsList(..),IsString(..))

import           Syntax (Expr(..),apply)
import           GenericInterpreter

data Val = NumVal | ClosureVal (Pow Expr) | Top deriving (Eq, Generic)
type Env = HashMap Text Val

eval0CFA :: State Label Expr -> Terminating (Error (Pow String) Val)
eval0CFA e =
  run (eval ::
        Fix Expr Val
         (ValueT
           (EnvT Text Val
             (ErrorT (Pow String)
               (TerminatingT
                 (FixT _ _
                   (S.ChaoticT _ _ 
                     (->))))))) Expr Val)
    iterationStrategy
    (empty,generate e)
  where
    iterationStrategy = S.filter apply
                      $ S.chaotic (W.finite)

newtype ValueT c x y = ValueT {runValueT :: c x y}  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowFail e,ArrowComplete,ArrowEnv var val)

instance ArrowRun c => ArrowRun (ValueT c) where
  type Rep (ValueT c) x y = Rep c x y
  run = run . runValueT

instance ArrowTrans ValueT where
  type Dom ValueT x y = x
  type Cod ValueT x y = y
  lift = ValueT
  unlift = runValueT

type instance Fix x y (ValueT c) = ValueT (Fix x y c)
deriving instance ArrowFix (Dom ValueT x y) (Cod ValueT x y) c => ArrowFix x y (ValueT c)
instance (IsString e, ArrowChoice c, ArrowFail e c, ArrowComplete c) => IsNum Val (ValueT c) where
  type Join (ValueT c) x y = Complete y

  succ = proc x -> case x of
    Top -> (returnA -< NumVal) <⊔> (fail -< "Expected a number as argument for 'succ'")
    NumVal -> returnA -< NumVal
    ClosureVal _ -> fail -< "Expected a number as argument for 'succ'"

  pred = proc x -> case x of
    Top -> (returnA -< NumVal) <⊔> (fail -< "Expected a number as argument for 'pred'")
    NumVal -> returnA -< NumVal
    ClosureVal _ -> fail -< "Expected a number as argument for 'pred'"

  zero = proc _ -> returnA -< NumVal

  if_ f g = proc v -> case v of
    (Top, (x,y)) -> (f -< x) <⊔> (g -< y) <⊔> (fail -< "Expected a number as condition for 'ifZero'")
    (NumVal, (x, y)) -> (f -< x) <⊔> (g -< y) -- case the interval contains zero and other numbers.
    (ClosureVal _, _)      -> fail -< "Expected a number as condition for 'ifZero'"

instance (IsString e, ArrowChoice c, ArrowFail e c, ArrowLowerBounded c, ArrowComplete c, ArrowEnv var val c)
    => IsClosure Val (ValueT c) where
  closure = arr $ \e -> ClosureVal (singleton e)
  applyClosure (ValueT f) = ValueT $ proc (fun, arg) -> case fun of
    Top -> fail -< "Expected a closure"
    ClosureVal cls -> do
      joinList1 (proc ((),(e,arg)) -> f -< (e,arg)) -< ((),(toList cls,arg))
    NumVal -> fail -< "Expected a closure"

instance PreOrd Val where
  _ ⊑ Top = True
  NumVal ⊑ NumVal = True
  ClosureVal c1 ⊑ ClosureVal c2 = c1 ⊑ c2
  _ ⊑ _ = False

instance Complete Val where
  v1 ⊔ v2 = snd $ widening v1 v2

widening :: W.Widening Val
widening NumVal NumVal = (W.Stable,NumVal)
widening (ClosureVal cs) (ClosureVal cs') = ClosureVal <$> W.finite cs cs'
widening Top Top = (W.Stable,Top)
widening _ _ = (W.Instable,Top)

instance UpperBounded Val where
  top = Top

instance Hashable Val
instance Show Val where
  show NumVal = "NumVal"
  show (ClosureVal cls) = show cls
  show Top = "⊤"
