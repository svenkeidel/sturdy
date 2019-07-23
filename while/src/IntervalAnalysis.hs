{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-partial-type-signatures #-}
-- | Interval Analysis for the While language.
module IntervalAnalysis where

import           Prelude hiding (Bool(..),Bounded(..),(/),fail,(.))
import qualified Prelude as P

import           Syntax
import           GenericInterpreter
import qualified GenericInterpreter as Generic

import           Data.Abstract.Boolean (Bool)
import qualified Data.Abstract.Boolean as B
import qualified Data.Abstract.Failure as F
import           Data.Abstract.Error (Error(..))
import qualified Data.Abstract.Error as E
import           Data.Abstract.Interval (Interval)
import qualified Data.Abstract.Interval as I
import qualified Data.Abstract.StrongMap as SM
import qualified Data.Abstract.Map as M
import           Data.Abstract.Terminating (Terminating)
import qualified Data.Abstract.Terminating as T
import           Data.Abstract.Widening (Widening)
import           Data.Abstract.FreeCompletion(FreeCompletion)
import           Data.Abstract.InfiniteNumbers
import           Data.Abstract.DiscretePowerset (Pow)
import qualified Data.Abstract.Widening as W
import qualified Data.Abstract.StackWidening as SW
import qualified Data.Abstract.Ordering as O
import qualified Data.Abstract.Equality as E

import qualified Data.Lens as L
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
import           Control.Arrow.Environment
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Random
import           Control.Arrow.Store
import           Control.Arrow.Order
import           Control.Arrow.Trans (ArrowRun)
import qualified Control.Arrow.Trans as Trans

import           Control.Arrow.Transformer.Abstract.Environment
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Store
import           Control.Arrow.Transformer.Abstract.Terminating
import qualified Control.Arrow.Transformer.Abstract.Fix.IterationStrategy as S

import           GHC.Exts(IsString(..))
import           GHC.Generics

-- | Abstract values are either abstract booleans or intervals.
data Val = BoolVal Bool | NumVal IV | Top deriving (Eq,Generic)
type IV = Interval (InfiniteNumber Int)
type Addr = FreeCompletion Label

-- | The interval analysis instantiates the generic interpreter
-- 'Generic.run' with the components for fixpoint computation
-- ('FixT'), termination ('TerminatingT'), failure ('ErrorT'), store
-- ('StoreT'), environments ('EnvT'), and values ('IntervalT').
run :: (?bound :: IV) => Int -> [(Text,Addr)] -> [LStatement] -> Terminating (Error (Pow String) (M.Map Addr Val))
run k env ss = fmap fst <$>
  Trans.run
    (Generic.run ::
      Fix [Statement] ()
        (IntervalT
          (EnvT Text Addr
            (StoreT Addr Val
              (ErrorT (Pow String)
                (TerminatingT
                  (FixT _ _
                    (S.StackWideningT _ _
                      (S.ChaoticT _ _
                         (->))))))))) [Statement] ())
      iterationStrategy
      (M.empty,(SM.fromList env, generate (sequence ss)))

  where
    iterationStrategy = S.filter (L.second (L.second whileLoops))
                      $ S.stackWidening stackWiden 
                      $ S.chaotic widenResult

    stackWiden = SW.groupBy (L.iso (\(store,(ev,stmts)) -> (stmts,(ev,store)))
                                   (\(stmts,(ev,store)) -> (store,(ev,stmts))))
               $ SW.maxSize k
               $ SW.reuseFirst
               $ SW.fromWidening (SM.widening W.finite W.** M.widening widenVal)
               $ SW.finite
    
    widenVal = widening (W.bounded ?bound I.widening)
    widenResult = T.widening $ E.widening W.finite (M.widening widenVal W.** W.finite)

newtype IntervalT c x y = IntervalT { runIntervalT :: c x y } deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowFail e,ArrowExcept exc,ArrowEnv var val env,ArrowStore var val,ArrowComplete,PreOrd,Complete)
type instance Fix x y (IntervalT c) = IntervalT (Fix x y c)
deriving instance ArrowFix x y c => ArrowFix x y (IntervalT c)

instance ArrowRun c => ArrowRun (IntervalT c) where
  type Rep (IntervalT c) x y = Trans.Rep c x y
  run = Trans.run . runIntervalT

instance (ArrowChoice c, Profunctor c) => ArrowAlloc (Text,Val,Label) Addr (IntervalT c) where
  alloc = arr $ \(_,_,l) -> return l

instance (IsString e, ArrowChoice c, ArrowFail e c, ArrowComplete c) => IsVal Val (IntervalT c) where
  type JoinVal (IntervalT c) (x,y) z = Complete z

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
      F.Fail e     -> fail -< (fromString e)
      F.Success n3 -> returnA -< NumVal n3
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
  (⊔) = W.toJoin widening (⊔)

widening :: Widening IV -> Widening Val
widening w v1 v2 = case (v1,v2) of
  (BoolVal b1,BoolVal b2) -> second BoolVal (B.widening b1 b2)
  (NumVal n1,NumVal n2) -> second NumVal (n1 `w` n2)
  (Top,Top) -> (W.Stable,Top)
  (_,_) -> (W.Instable,Top)

instance Show Val where
  show (NumVal iv) = show iv
  show (BoolVal b) = show b
  show Top = "⊤"

instance Hashable Val
