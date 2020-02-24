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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-partial-type-signatures #-}
-- | Interval Analysis for the While language.
module IntervalAnalysis where

import           Prelude hiding (Bool(..),Bounded(..),(/),fail,(.),filter)
import qualified Prelude as P

import           Syntax
import           GenericInterpreter
import qualified GenericInterpreter as Generic

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fail as Fail
import           Control.Arrow.Fix
import           Control.Arrow.Fix.Context
import           Control.Arrow.Fix.Chaotic(innermost)
import           Control.Arrow.Random
import           Control.Arrow.Order
import qualified Control.Arrow.Trans as Trans

import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.Environment
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Except
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Component
import           Control.Arrow.Transformer.Abstract.Fix.Context
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable hiding (Widening)
import           Control.Arrow.Transformer.Abstract.Fix.Stack
import           Control.Arrow.Transformer.Abstract.Store
import           Control.Arrow.Transformer.Abstract.Terminating

import           Data.Profunctor
import qualified Data.Boolean as B
import           Data.Hashable
import           Data.Numeric
import           Data.Order
import           Data.Label
import           Data.Text (Text)
import           Data.Utils

import           Data.Abstract.Boolean (Bool)
import qualified Data.Abstract.Boolean as B
import           Data.Abstract.DiscretePowerset (Pow)
import qualified Data.Abstract.Equality as E
import           Data.Abstract.Error (Error(..))
import qualified Data.Abstract.Error as E
import           Data.Abstract.Except (Except(..))
import qualified Data.Abstract.Except as Exc
import qualified Data.Abstract.Failure as F
import           Data.Abstract.FreeCompletion(FreeCompletion)
import           Data.Abstract.InfiniteNumbers
import           Data.Abstract.Interval (Interval)
import qualified Data.Abstract.Interval as I
import           Data.Abstract.Map (Map)
import qualified Data.Abstract.Map as M
import qualified Data.Abstract.Maybe as AM
import qualified Data.Abstract.Ordering as O
import qualified Data.Abstract.StrongMap as SM
import           Data.Abstract.Terminating (Terminating)
import qualified Data.Abstract.Terminating as T
import           Data.Abstract.Stable
import           Data.Abstract.Widening (Widening)
import qualified Data.Abstract.Widening as W
import           Data.Abstract.CallString (CallString)

import           GHC.Exts(IsString(..))
import           GHC.Generics
import           Text.Printf

-- | Abstract values are either abstract booleans or intervals.
data Val = BoolVal Bool | NumVal IV | TypeError (Pow String) deriving (Eq,Generic)
type IV = Interval (InfiniteNumber Int)
type Addr = FreeCompletion Label
type Env = SM.Map Text Addr
type Store = Map Addr Val
newtype Exception = Exception (Map Text Val) deriving (PreOrd,Complete,Show,Eq)

-- | The interval analysis instantiates the generic interpreter
-- 'Generic.run' with the components for fixpoint computation
-- ('FixT'), termination ('TerminatingT'), failure ('ErrorT'), store
-- ('StoreT'), environments ('EnvT'), and values ('IntervalT').
run :: (?bound :: IV) => Int -> [(Text,Addr)] -> [LStatement] -> Terminating (Error (Pow String) (Except Exception (M.Map Addr Val)))
run k env ss = fmap (fmap (fmap fst)) <$> snd $
  Trans.run
    (Generic.run ::
      Fix'
        (ValueT Val
          (EnvT Env
            (StoreT Store
              (ExceptT Exception
                (ErrorT (Pow String)
                  (TerminatingT
                    (FixT _ _
                      (ComponentT _
                        (StackT Stack _
                          (CacheT (Context (Proj2 (CtxCache (CallString lab))) (Group Cache)) (_,_) _
                            (ContextT (CallString _)
                               (->)))))))))))) [Statement] ())
      iterationStrategy
      (widenEnvStore,widenResult)
      (M.empty,(SM.fromList env, generate (sequence ss)))
  where
    iterationStrategy
      = filter whileLoops
      $ callsiteSensitive @(((Expr,Statement,Label),[Statement]),(_,_)) k (thrd . fst . fst)
      . innermost

    widenEnvStore = M.widening widenVal W.** SM.widening W.finite
    widenVal = widening (I.bounded ?bound)
    widenExc (Exception m1) (Exception m2) = Exception <$> M.widening widenVal m1 m2
    widenResult = T.widening $ E.widening W.finite (Exc.widening widenExc (M.widening widenVal W.** W.finite))
    thrd (_,_,z) = z

deriving instance ArrowComplete () c => ArrowComplete () (ValueT Val c)

instance (ArrowChoice c, Profunctor c) => ArrowAlloc Addr (ValueT Val c) where
  alloc = arr $ \(_,_,l) -> return l

instance (IsString e, ArrowChoice c, ArrowFail e c) => IsVal Val (ValueT Val c) where
  type JoinVal z (ValueT Val c) = ArrowComplete z (ValueT Val c)

  boolLit = arr $ \b -> case b of
    P.True -> BoolVal B.True
    P.False -> BoolVal B.False
  and = proc (v1,v2) -> case (v1,v2) of
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 `B.and` b2)
    _                       -> fail -< "Expected two booleans as arguments for 'and'"
  or = proc (v1,v2) -> case (v1,v2) of
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 `B.or` b2)
    _                       -> fail -< "Expected two booleans as arguments for 'or'"
  not = proc v -> case v of
    BoolVal b -> returnA -< BoolVal (B.not b)
    _         -> fail -< "Expected a boolean as argument for 'not'"
  numLit = proc x -> returnA -< NumVal (I.Interval (Number x) (Number x))
  add = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 + n2)
    _                     -> fail -< "Expected two numbers as arguments for 'add'"
  sub = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 - n2)
    _                     -> fail -< "Expected two numbers as arguments for 'sub'"
  mul = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 * n2)
    _                     -> fail -< "Expected two numbers as arguments for 'mul'"
  div = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> case n1 / n2 of
      F.Fail e     -> fail -< (fromString e)
      F.Success n3 -> returnA -< NumVal n3
    _              -> fail -< "Expected two numbers as arguments for 'mul'"
  eq = proc (v1,v2) -> case (v1,v2) of
    (NumVal x,NumVal y)     -> returnA -< BoolVal (x E.== y)
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 E.== b2)
    _                       -> fail -< "Expected two values of the same type as arguments for 'eq'"
  lt = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< BoolVal (n1 O.< n2)
    _                     -> fail -< "Expected two numbers as arguments for 'lt'"
  if_ f1 f2 = proc (v,(x,y)) -> case v of
    BoolVal B.True  -> f1 -< x
    BoolVal B.False -> f2 -< y
    BoolVal B.Top   -> (f1 -< x) <⊔> (f2 -< y)
    _               -> fail -< "Expected boolean as argument for 'if'"

instance ArrowChoice c => IsException Exception Val (ValueT Val c) where
  type JoinExc y (ValueT Val c) = ArrowComplete y (ValueT Val c)
  namedException = proc (name,val) -> returnA -< Exception (M.singleton name val)
  matchException f g = proc (name,Exception m,x) -> case M.lookup name m of
    AM.Just v        -> f -< (v,x)
    AM.Nothing       -> g -< x
    AM.JustNothing v -> (f -< (v,x)) <⊔> (g -< x)

instance (ArrowChoice c, Profunctor c) => ArrowRand Val (ValueT Val c) where
  random = proc _ -> returnA -< NumVal top

instance PreOrd Val where
  _ ⊑ TypeError _ = P.True
  BoolVal b1 ⊑ BoolVal b2 = b1 P.== b2
  NumVal n1 ⊑ NumVal n2 = n1 ⊑ n2
  _ ⊑ _ = P.False

instance Complete Val where
  (⊔) = W.toJoin widening (⊔)

widening :: Widening IV -> Widening Val
widening w v1 v2 = case (v1,v2) of
  (BoolVal b1,BoolVal b2) -> second BoolVal (B.widening b1 b2)
  (NumVal n1,NumVal n2) -> second NumVal (n1 `w` n2)
  (NumVal _,BoolVal _) -> (Unstable, TypeError (singleton "Cannot unify a number with a boolean"))
  (BoolVal _,NumVal _) -> (Unstable, TypeError (singleton "Cannot unify a boolean with a number"))
  (TypeError m1,TypeError m2) -> (Stable,TypeError (m1 <> m2))
  (_,TypeError m2) -> (Unstable,TypeError m2)
  (TypeError m1,_) -> (Unstable,TypeError m1)

instance Show Val where
  show (NumVal iv) = show iv
  show (BoolVal b) = show b
  show (TypeError m) = printf "TypeError: " (show m)

instance Hashable Val
