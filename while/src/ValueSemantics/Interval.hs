{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ValueSemantics.Interval where

import           Prelude hiding (Bool(..),Bounded(..),(==),(/),(<))
import qualified Prelude as P

import           Syntax
import           SharedSemantics
import qualified SharedSemantics as Shared

import           Data.Abstract.Boolean (Bool)
import qualified Data.Abstract.Boolean as B
import           Data.Abstract.Bounded hiding (lift)
import           Data.Abstract.Error (Error(..))
import           Data.Abstract.Interval (Interval)
import qualified Data.Abstract.Interval as I
import           Data.Abstract.Store (Store)
import qualified Data.Abstract.Store as S
import           Data.Abstract.Terminating
import           Data.Abstract.Widening
import           Data.Abstract.Ordering
import           Data.Abstract.Equality

import qualified Data.Boolean as B
import           Data.Hashable
import           Data.Numeric
import           Data.Order
import           Data.Label
import           Data.Text (Text)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Store
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Abstract.Except
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Store
import           Control.Monad.State

import           GHC.Generics

type IV = Interval Int
data Val = BoolVal Bool | NumVal (Bounded IV) | Top deriving (Eq,Generic)

newtype Interp c x y = Interp (Const IV (StoreArrow Text Val (Except String c)) x y)
type instance Fix x y (Interp c) = Interp (Fix (Store Text Val,x) (Error String (Store Text Val,y)) c)

runInterp :: IV -> Interp c x y -> c (Store Text Val,x) (Error String (Store Text Val,y))
runInterp b (Interp f) = runExcept (runStore (runConst b f))

run :: (?bound :: IV) => [State Label Statement] -> Terminating (Error String (Store Text Val))
run ss = fmap fst <$> runFix (runInterp ?bound (Shared.run :: Fix [Statement] () (Interp (~>)) [Statement] ())) (S.empty,generate (sequence ss))

instance ArrowChoice c => IsVal Val (Interp c) where
  boolLit = arr $ \(b,_) -> case b of
    P.True -> BoolVal B.True
    P.False -> BoolVal B.False
  and = proc (v1,v2,_) -> case (v1,v2) of
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 `B.and` b2)
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> failA -< "Expected two booleans as arguments for 'and'"
  or = proc (v1,v2,_) -> case (v1,v2) of
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 `B.or` b2)
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> failA -< "Expected two booleans as arguments for 'ord'"
  not = proc (v,_) -> case v of
    BoolVal b -> returnA -< BoolVal (B.not b)
    Top -> returnA -< Top
    _ -> failA -< "Expected a boolean as argument for 'not'"
  numLit = proc (x,_) -> do
    b <- askConst -< ()
    returnA -< NumVal (Bounded b (I.Interval x x))
  randomNum = proc _ -> do
    b <- askConst -< ()
    returnA -< NumVal (Bounded b top)
  add = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 + n2)
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> failA -< "Expected two numbers as arguments for 'add'"
  sub = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 - n2)
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> failA -< "Expected two numbers as arguments for 'sub'"
  mul = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 * n2)
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> failA -< "Expected two numbers as arguments for 'mul'"
  div = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> case n1 / n2 of
      Fail e -> failA -< e
      Success n3 -> returnA -< NumVal n3
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> failA -< "Expected two numbers as arguments for 'mul'"
  eq = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal x,NumVal y) -> returnA -< BoolVal (x == y)
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 == b2)
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> failA -< "Expected two values of the same type as arguments for 'eq'"
  lt = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2)   -> returnA -< BoolVal (n1 < n2)
    (Top,_)   -> returnA -< Top
    (_,Top)   -> returnA -< Top
    _ -> failA -< "Expected two numbers as arguments for 'lt'"

instance (Complete (c (Store Text Val,(x,y)) (Error String (Store Text Val,z))), ArrowChoice c)
  => Conditional Val x y z (Interp c) where
  if_ f1 f2 = proc (v,(x,y)) -> case v of
    BoolVal B.True -> f1 -< x
    BoolVal B.False -> f2 -< y
    BoolVal B.Top -> joined f1 f2 -< (x,y)
    Top -> joined f1 f2 -< (x,y)
    _ -> failA -< "Expected boolean as argument for 'if'"


deriving instance ArrowChoice c => Category (Interp c)
deriving instance ArrowChoice c => Arrow (Interp c)
deriving instance ArrowChoice c => ArrowChoice (Interp c)
deriving instance (ArrowChoice c, ArrowLoop c) => ArrowLoop (Interp c)
deriving instance ArrowChoice c => ArrowFail String (Interp c)
deriving instance ArrowChoice c => ArrowConst IV (Interp c)
deriving instance (ArrowChoice c, ArrowFix (Store Text Val,x) (Error String (Store Text Val,y)) c) => ArrowFix x y (Interp c)
deriving instance (Complete (c ((Store Text Val,Val),Text) (Error String (Store Text Val,Val))), ArrowChoice c) => ArrowStore Text Val (Interp c)
deriving instance (PreOrd (c (Store Text Val,x) (Error String (Store Text Val,y)))) => PreOrd (Interp c x y)
deriving instance (Complete (c (Store Text Val,x) (Error String (Store Text Val,y)))) => Complete (Interp c x y)

instance PreOrd Val where
  _ ⊑ Top = P.True
  BoolVal b1 ⊑ BoolVal b2 = b1 P.== b2
  NumVal n1 ⊑ NumVal n2 = n1 ⊑ n2
  _ ⊑ _ = P.False

instance UpperBounded Val where
  top = Top

instance Complete Val where
  BoolVal b1 ⊔ BoolVal b2 = BoolVal $ b1 ⊔ b2
  NumVal n1 ⊔ NumVal n2 = NumVal $ n1 ⊔ n2
  _ ⊔ _ = Top

instance Widening Val where
  BoolVal b1 ▽ BoolVal b2 = BoolVal (b1 ⊔ b2)
  NumVal n1 ▽ NumVal n2 = NumVal (n1 ▽ n2)
  _ ▽ _ = Top

-- instance Galois (IV -> Pow Concrete.Val) (IV -> Val) where
--   alpha x = \b -> let ?bound = b in lifted lift (x b)
--     where lift (Concrete.BoolVal b) = BoolVal (alphaSing b)
--           lift (Concrete.NumVal n) = NumVal $ bounded (I.Interval n n)
--   gamma x b = case x b of
--     BoolVal y -> Concrete.BoolVal <$> gamma y
--     NumVal (Bounded _ y) -> Concrete.NumVal <$> gamma y
--     Top -> gamma (\(_::IV) -> BoolVal B.Top) b `union` gamma (\(_::IV) -> (NumVal (Bounded b top))) b

instance Show Val where
  show (NumVal iv) = show iv
  show (BoolVal b) = show b
  show Top = "⊤"

instance Hashable Val
