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
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ValueSemantics.Interval where

import           Prelude hiding (Bool(..),Bounded(..),(==),(/),fail)
import qualified Prelude as P

import           Syntax
import           SharedSemantics
import qualified SharedSemantics as Shared
import           ValueSemantics.Abstract

import           Data.Abstract.Boolean (Bool)
import qualified Data.Abstract.Boolean as B
import           Data.Abstract.PropagateError (Error(..))
import           Data.Abstract.Interval (Interval)
import qualified Data.Abstract.Interval as I
import           Data.Abstract.PreciseStore (Store)
import qualified Data.Abstract.PreciseStore as S
import qualified Data.Abstract.Environment as E
import           Data.Abstract.Terminating
import           Data.Abstract.Widening
import qualified Data.Abstract.Ordering as O
import           Data.Abstract.Equality
import           Data.Abstract.Bounded

import qualified Data.Boolean as B
import           Data.Hashable
import           Data.Numeric
import           Data.Order
import           Data.Label
import           Data.Text (Text)

import           Control.Arrow
import           Control.Arrow.Alloc
import           Control.Arrow.Const
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Conditional
import           Control.Arrow.Random

import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Abstract.LeastFixPoint

import           GHC.Generics

type Addr = Label
type IV = Interval Int
data Val = BoolVal Bool | NumVal (Bounded IV) | Top deriving (Eq,Generic)

run :: (?bound :: IV) => [(Text,Addr)] -> [LStatement] -> Terminating (Error String (Store Addr Val))
run env ss =
  fmap fst <$>
    runLeastFix
      (runConst ?bound
        (runInterp
          (Shared.run :: Fix [Statement] () (Interp Addr Val (Const IV (LeastFix () () (->)))) [Statement] ())))
      (S.empty,(E.fromList env, generate <$> ss))

instance ArrowChoice c => ArrowAlloc (Text,Val,Label) Addr (Interp Addr Val c) where
  alloc = arr $ \(_,_,l) -> l

instance (ArrowChoice c, ArrowConst IV c) => IsVal Val (Interp Addr Val c) where
  boolLit = arr $ \(b,_) -> case b of
    P.True -> BoolVal B.True
    P.False -> BoolVal B.False
  and = proc (v1,v2,_) -> case (v1,v2) of
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 `B.and` b2)
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> fail -< "Expected two booleans as arguments for 'and'"
  or = proc (v1,v2,_) -> case (v1,v2) of
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 `B.or` b2)
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> fail -< "Expected two booleans as arguments for 'ord'"
  not = proc (v,_) -> case v of
    BoolVal b -> returnA -< BoolVal (B.not b)
    Top -> returnA -< Top
    _ -> fail -< "Expected a boolean as argument for 'not'"
  numLit = proc (x,_) -> do
    b <- askConst -< ()
    returnA -< NumVal (Bounded b (I.Interval x x))
  add = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 + n2)
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> fail -< "Expected two numbers as arguments for 'add'"
  sub = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 - n2)
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> fail -< "Expected two numbers as arguments for 'sub'"
  mul = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 * n2)
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> fail -< "Expected two numbers as arguments for 'mul'"
  div = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> case n1 / n2 of
      Fail e -> fail -< e
      Success n3 -> returnA -< NumVal n3
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> fail -< "Expected two numbers as arguments for 'mul'"
  eq = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal x,NumVal y) -> returnA -< BoolVal (x == y)
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 == b2)
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> fail -< "Expected two values of the same type as arguments for 'eq'"
  lt = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< BoolVal (n1 O.< n2)
    (Top,_)   -> returnA -< Top
    (_,Top)   -> returnA -< Top
    _ -> fail -< "Expected two numbers as arguments for 'lt'"

instance (Complete (Interp Addr Val c (x,y) z), UpperBounded z, ArrowChoice c)
  => ArrowCond Val x y z (Interp Addr Val c) where
  if_ f1 f2 = proc (v,(x,y)) -> case v of
    BoolVal B.True -> f1 -< x
    BoolVal B.False -> f2 -< y
    BoolVal B.Top -> joined f1 f2 -< (x,y)
    Top -> returnA -< top
    _ -> fail -< "Expected boolean as argument for 'if'"

instance (ArrowConst IV (Interp addr val c), ArrowChoice c) => ArrowRand Val (Interp addr val c) where
  random = proc _ -> do
    b <- askConst -< ()
    returnA -< NumVal (Bounded b top)

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
