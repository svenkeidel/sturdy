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

import           Prelude hiding (Bool(..),Bounded(..),(==),(/),(<))
import qualified Prelude as P

import           Syntax
import           SharedSemantics
import qualified SharedSemantics as Shared

import           Data.Abstract.Error (Error(..))
import           Data.Abstract.Store (Store)
import qualified Data.Abstract.Store as S
import           Data.Abstract.Terminating
import           Data.Abstract.Widening

import           Data.Hashable
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

import           GHC.Generics

data Val = Val Expr | Top deriving (Eq,Generic)

newtype Interp c x y = Interp (StoreArrow Text Val (Except String c) x y)
type instance Fix x y (Interp c) = Interp (Fix (Store Text Val,x) (Error String (Store Text Val,y)) c)

runInterp :: Interp c x y -> c (Store Text Val,x) (Error String (Store Text Val,y))
runInterp (Interp f) = runExcept (runStore f)

run :: [State Label Statement] -> Terminating (Error String (Store Text Val))
run ss = fmap fst <$> runLeastFixPoint (runInterp (Shared.run :: Fix [Statement] () (Interp (~>)) [Statement] ())) (S.empty,generate (sequence ss))

instance ArrowChoice c => IsVal Val (Interp c) where
  boolLit = arr $ \(b,l) -> Val (BoolLit b l)
  and = proc (v1,v2,l) -> case (v1,v2) of
    (Val b1,Val b2) -> returnA -< Val (And b1 b2 l)
    (_,_) -> returnA -< Top
  or = proc (v1,v2,l) -> case (v1,v2) of
    (Val b1,Val b2) -> returnA -< Val (Or b1 b2 l)
    (_,_) -> returnA -< Top
  not = proc (v,l) -> case v of
    Val b -> returnA -< Val (Not b l)
    Top -> returnA -< Top
  numLit = arr $ \(n,l) -> Val (NumLit n l)
  randomNum = arr $ \l -> Val (RandomNum l)
  add = proc (v1,v2,l) -> case (v1,v2) of
    (Val n1,Val n2) -> returnA -< Val (Add n1 n2 l)
    (_,_) -> returnA -< Top
  sub = proc (v1,v2,l) -> case (v1,v2) of
    (Val n1,Val n2) -> returnA -< Val (Sub n1 n2 l)
    (_,_) -> returnA -< Top
  mul = proc (v1,v2,l) -> case (v1,v2) of
    (Val n1,Val n2) -> returnA -< Val (Mul n1 n2 l)
    (_,_) -> returnA -< Top
  div = proc (v1,v2,l) -> case (v1,v2) of
    (Val n1,Val n2) -> returnA -< Val (Div n1 n2 l)
    (_,_) -> returnA -< Top
  eq = proc (v1,v2,l) -> case (v1,v2) of
    (Val x,Val y) -> returnA -< Val (Eq x y l)
    (_,_) -> returnA -< Top
  lt = proc (v1,v2,l) -> case (v1,v2) of
    (Val n1,Val n2) -> returnA -< Val (Lt n1 n2 l)
    (_,_)   -> returnA -< Top

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

instance PreOrd Val where
  _ ⊑ Top = P.True
  Val n1 ⊑ Val n2 = n1 P.== n2
  _ ⊑ _ = P.False

instance UpperBounded Val where
  top = Top

instance Complete Val where
  Val e1 ⊔ Val e2 | e1 P.== e2 = Val e1
  _ ⊔ _ = Top

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

instance Hashable Val
