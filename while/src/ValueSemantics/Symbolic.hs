{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ValueSemantics.Symbolic where

import           Prelude hiding (Bool(..),Bounded(..),(==),(/),(<),(.))
import qualified Prelude as P

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
import           Data.Abstract.Widening

import           Data.Hashable
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

import           GHC.Generics

type Addr = FreeCompletion (Pow Label)
data Val = Bot | Val Expr | Top deriving (Eq,Generic)
type Env = E.Env Text Addr
type Store = UncertainStore Label Val

newtype Interp c x y = Interp (Environment Text Addr (UncertainStoreArrow Label Val (Except String c)) x y)
type instance Fix x y (Interp c) = Interp (Fix (Store,(Env,x)) (Error String (Store,y)) c)

runInterp :: ArrowChoice c => Interp c x y -> c (Store,(Env,x)) (Error String (Store,y))
runInterp (Interp f) = runExcept (runUncertainStore (runEnvironment' f))

run :: [State Label Statement] -> Terminating (Error String Store)
run ss = fmap fst <$> runLeastFixPoint (runInterp (Shared.run :: Fix [Statement] () (Interp (~>)) [Statement] ())) ((S.empty,bottom),(E.empty, generate (sequence ss)))

instance ArrowChoice c => IsVal Val Addr (Interp c) where
  boolLit = arr $ \(b,l) -> Val (BoolLit b l)
  and = proc (v1,v2,l) -> case (v1,v2) of
    (Val b1,Val b2) -> returnA -< Val (And b1 b2 l)
    (Bot,Bot) -> returnA -< Bot
    (_,_) -> returnA -< Top
  or = proc (v1,v2,l) -> case (v1,v2) of
    (Val b1,Val b2) -> returnA -< Val (Or b1 b2 l)
    (Bot,Bot) -> returnA -< Bot
    (_,_) -> returnA -< Top
  not = proc (v,l) -> case v of
    Val b -> returnA -< Val (Not b l)
    Top -> returnA -< Top
    Bot -> returnA -< Bot
  numLit = arr $ \(n,l) -> Val (NumLit n l)
  randomNum = arr $ \l -> Val (RandomNum l)
  add = proc (v1,v2,l) -> case (v1,v2) of
    (Val n1,Val n2) -> returnA -< Val (Add n1 n2 l)
    (Bot,Bot) -> returnA -< Bot
    (_,_) -> returnA -< Top
  sub = proc (v1,v2,l) -> case (v1,v2) of
    (Val n1,Val n2) -> returnA -< Val (Sub n1 n2 l)
    (Bot,Bot) -> returnA -< Bot
    (_,_) -> returnA -< Top
  mul = proc (v1,v2,l) -> case (v1,v2) of
    (Val n1,Val n2) -> returnA -< Val (Mul n1 n2 l)
    (Bot,Bot) -> returnA -< Bot
    (_,_) -> returnA -< Top
  div = proc (v1,v2,l) -> case (v1,v2) of
    (Val n1,Val n2) -> returnA -< Val (Div n1 n2 l)
    (Bot,Bot) -> returnA -< Bot
    (_,_) -> returnA -< Top
  eq = proc (v1,v2,l) -> case (v1,v2) of
    (Val x,Val y) -> returnA -< Val (Eq x y l)
    (Bot,Bot) -> returnA -< Bot
    (_,_) -> returnA -< Top
  lt = proc (v1,v2,l) -> case (v1,v2) of
    (Val n1,Val n2) -> returnA -< Val (Lt n1 n2 l)
    (Bot,Bot) -> returnA -< Bot
    (_,_)   -> returnA -< Top
  freshAddr = arr $ FC.Lower . Pow.singleton
  ref = arr (const Top)
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

instance PreOrd Val where
  _ ⊑ Top = P.True
  Val n1 ⊑ Val n2 = n1 P.== n2
  _ ⊑ _ = P.False

instance UpperBounded Val where
  top = Top

instance Complete Val where
  Val e1 ⊔ Val e2 | e1 P.== e2 = Val e1
  _ ⊔ _ = Top

instance LowerBounded Val where
  bottom = Bot

instance Widening Val

-- instance Galois (IV -> Pow Concrete.Val) (IV -> Val) where
--   alpha x = \b -> let ?bound = b in lifted lift (x b)
--     where lift (Concrete.BoolVal b) = BoolVal (alphaSing b)
--           lift (Concrete.NumVal n) = NumVal $ bounded (I.Interval n n)
--   gamma x b = case x b of
--     BoolVal y -> Concrete.BoolVal <$> gamma y
--     NumVal (Bounded _ y) -> Concrete.NumVal <$> gamma y
--     Top -> gamma (\(_::IV) -> BoolVal B.Top) b `union` gamma (\(_::IV) -> (NumVal (Bounded b top))) b

instance Show Val where
  show (Val e) = show e
  show Top = "⊤"
  show Bot = "⊥"

instance Hashable Val
