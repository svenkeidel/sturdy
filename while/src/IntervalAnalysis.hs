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
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-partial-type-signatures #-}
module IntervalAnalysis where

import           Prelude hiding (Bool(..),Bounded(..),(/),fail)
import qualified Prelude as P

import           Syntax
import           GenericInterpreter
import qualified GenericInterpreter as Generic

import           Data.Abstract.Boolean (Bool)
import qualified Data.Abstract.Boolean as B
import           Data.Abstract.Failure (Failure(..))
import qualified Data.Abstract.Failure as F
import           Data.Abstract.Interval (Interval)
import qualified Data.Abstract.Interval as I
import           Data.Abstract.Map (Map)
import qualified Data.Abstract.Map as M
import           Data.Abstract.Terminating hiding(widening)
import           Data.Abstract.Widening (Widening)
import           Data.Abstract.FreeCompletion(FreeCompletion)
import           Data.Abstract.InfiniteNumbers
import           Data.Monoidal (Iso(..))
import qualified Data.Abstract.Widening as W
import qualified Data.Abstract.StackWidening as S
import qualified Data.Abstract.Ordering as O
import qualified Data.Abstract.Equality as E

import           Data.Profunctor
import qualified Data.Boolean as B
import           Data.Hashable
import           Data.Numeric
import           Data.Order
import           Data.Label
import           Data.Text (Text)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Alloc
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Conditional
import           Control.Arrow.Environment
import           Control.Arrow.Store
import           Control.Arrow.Random
import           Control.Arrow.Abstract.Join

import           Control.Arrow.Transformer.Abstract.Fixpoint
import           Control.Arrow.Transformer.Abstract.Environment
import           Control.Arrow.Transformer.Abstract.Store
import           Control.Arrow.Transformer.Abstract.Failure

import           GHC.Generics

type Addr = FreeCompletion Label
type IV = Interval (InfiniteNumber Int)
data Val = BoolVal Bool | NumVal IV | Top deriving (Eq,Generic)

run :: (?bound :: IV) => Int -> [(Text,Addr)] -> [LStatement] -> Terminating (Failure String (Map Addr Val))
run k env ss =
  fmap fst <$>
    runFixT' stackWiden widenTerm
      (runFailureT
         (runStoreT
           (runEnvT
             (runIntervalT
               (Generic.run ::
                 Fix [Statement] ()
                   (IntervalT
                     (EnvT Text Addr
                       (StoreT Addr Val
                         (FailureT String
                          (FixT _ () () (->)))))) [Statement] ())))))
      (M.empty,(M.fromList env, generate <$> ss))

  where
    widenVal = widening (W.bounded ?bound top)
    widenTerm = F.widening (M.widening widenVal W.** W.finite)
    stackWiden = S.categorize (Iso (\(store,(ev,stmts)) -> ((ev,stmts),store)) (\((ev,stmts),store) -> (store,(ev,stmts))))
               $ S.stack
               $ S.maxSize k
               $ S.reuse (\_ l -> head l)
               $ S.fromWidening (M.widening widenVal)

newtype IntervalT c x y = IntervalT { runIntervalT :: c x y } deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowFail e,ArrowEnv var val env,ArrowStore var val,ArrowJoin,PreOrd,Complete)
type instance Fix x y (IntervalT c) = IntervalT (Fix x y c)
deriving instance ArrowFix x y c => ArrowFix x y (IntervalT c)

instance (ArrowChoice c, Profunctor c) => ArrowAlloc (Text,Val,Label) Addr (IntervalT c) where
  alloc = arr $ \(_,_,l) -> return l

instance (ArrowChoice c, ArrowFail String c, ArrowJoin c) => IsVal Val (IntervalT c) where
  boolLit = arr $ \(b,_) -> case b of
    P.True -> BoolVal B.True
    P.False -> BoolVal B.False
  and = proc (v1,v2,_) -> case (v1,v2) of
    (BoolVal b1,BoolVal b2)    -> returnA -< BoolVal (b1 `B.and` b2)
    _ | v1 == Top || v2 == Top -> (returnA -< BoolVal top) <⊔> (fail -< "Expected two booleans as arguments for 'and'")
    _                          -> fail -< "Expected two booleans as arguments for 'and'"
  or = proc (v1,v2,_) -> case (v1,v2) of
    (BoolVal b1,BoolVal b2)    -> returnA -< BoolVal (b1 `B.or` b2)
    _ | v1 == Top || v2 == Top -> (returnA -< BoolVal top) <⊔> (fail -< "Expected two booleans as arguments for 'or'")
    _                          -> fail -< "Expected two booleans as arguments for 'or'"
  not = proc (v,_) -> case v of
    BoolVal b                  -> returnA -< BoolVal (B.not b)
    Top                        -> (returnA -< BoolVal top) <⊔> (fail -< "Expected a boolean as argument for 'not'")
    NumVal _                   -> fail -< "Expected a boolean as argument for 'not'"
  numLit = proc (x,_) -> returnA -< NumVal (I.Interval (Number x) (Number x))
  add = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2)      -> returnA -< NumVal (n1 + n2)
    _ | v1 == Top || v2 == Top -> (returnA -< NumVal top) <⊔> (fail -< "Expected two numbers as arguments for 'add'")
    _                          -> fail -< "Expected two numbers as arguments for 'add'"
  sub = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2)      -> returnA -< NumVal (n1 - n2)
    _ | v1 == Top || v2 == Top -> (returnA -< NumVal top) <⊔> (fail -< "Expected two numbers as arguments for 'sub'")
    _                          -> fail -< "Expected two numbers as arguments for 'sub'"
  mul = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2)      -> returnA -< NumVal (n1 * n2)
    _ | v1 == Top || v2 == Top -> (returnA -< NumVal top) <⊔> (fail -< "Expected two numbers as arguments for 'mul'")
    _                          -> fail -< "Expected two numbers as arguments for 'mul'"
  div = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> case n1 / n2 of
      Fail e     -> fail -< e
      Success n3 -> returnA -< NumVal n3
    _ | v1 == Top || v2 == Top -> (returnA -< NumVal top) <⊔> (fail -< "Expected two numbers as arguments for 'mul'")
    _                          -> fail -< "Expected two numbers as arguments for 'mul'"
  eq = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal x,NumVal y)        -> returnA -< BoolVal (x E.== y)
    (BoolVal b1,BoolVal b2)    -> returnA -< BoolVal (b1 E.== b2)
    _ | v1 == Top || v2 == Top -> (returnA -< BoolVal top) <⊔> (fail -< "Expected two values of the same type as arguments for 'eq'")
    _                          -> fail -< "Expected two values of the same type as arguments for 'eq'"
  lt = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2)      -> returnA -< BoolVal (n1 O.< n2)
    _ | v1 == Top || v2 == Top -> (returnA -< BoolVal top) <⊔> (fail -< "Expected two numbers as arguments for 'lt'")
    _                          -> fail -< "Expected two numbers as arguments for 'lt'"

instance (ArrowChoice c,ArrowFail String c, ArrowJoin c) => ArrowCond Val (IntervalT c) where
  type Join (IntervalT c) (x,y) z = Complete z
  if_ f1 f2 = proc (v,(x,y)) -> case v of
    BoolVal B.True  -> f1 -< x
    BoolVal B.False -> f2 -< y
    BoolVal B.Top   -> (f1 -< x) <⊔> (f2 -< y)
    NumVal _        -> fail -< "Expected boolean as argument for 'if'"
    Top             -> (f1 -< x) <⊔> (f2 -< y) <⊔> (fail -< "Expected boolean as argument for 'if'")

instance (ArrowChoice c, Profunctor c) => ArrowRand Val (IntervalT c) where
  random = proc _ -> returnA -< NumVal top

instance PreOrd Val where
  _ ⊑ Top = P.True
  BoolVal b1 ⊑ BoolVal b2 = b1 P.== b2
  NumVal n1 ⊑ NumVal n2 = n1 ⊑ n2
  _ ⊑ _ = P.False

instance UpperBounded Val where
  top = Top

instance Complete Val where
  (⊔) = widening (⊔)

widening :: Widening IV -> Widening Val
widening w v1 v2 = case (v1,v2) of
  (BoolVal b1,BoolVal b2) -> BoolVal (b1 ⊔ b2)
  (NumVal n1,NumVal n2) -> NumVal (n1 `w` n2)
  (_,_) -> Top

instance Show Val where
  show (NumVal iv) = show iv
  show (BoolVal b) = show b
  show Top = "⊤"

instance Hashable Val
