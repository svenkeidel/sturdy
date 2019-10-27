{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-partial-type-signatures #-}
-- | k-CFA analysis for PCF where numbers are approximated by intervals.
module IntervalAnalysis where

import           Prelude hiding (Bounded,fail,(.),exp)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Arrow.Fix as Fix
import           Control.Arrow.Trans
import           Control.Arrow.Closure (ArrowClosure)
import qualified Control.Arrow.Closure as Cls
import           Control.Arrow.Order
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.Environment
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Chaotic
import           Control.Arrow.Transformer.Abstract.Fix.Context
import           Control.Arrow.Transformer.Abstract.Fix.Stack
import           Control.Arrow.Transformer.Abstract.Fix.Cache
import           Control.Arrow.Transformer.Abstract.Terminating

import           Control.Monad.State hiding (lift,fail)

import           Data.Hashable
import           Data.Label
import           Data.Order
import           Data.Text (Text)
import           Data.Utils
import           Data.Profunctor
import           Data.Traversable

import qualified Data.Abstract.Environment.Flat as F
import           Data.Abstract.Error (Error)
import qualified Data.Abstract.Error as E
import           Data.Abstract.InfiniteNumbers
import           Data.Abstract.Interval (Interval)
import qualified Data.Abstract.Interval as I
import qualified Data.Abstract.IntersectionSet as S
import           Data.Abstract.Widening (Widening)
import qualified Data.Abstract.Widening as W
import           Data.Abstract.Stable
import           Data.Abstract.Terminating(Terminating)
import qualified Data.Abstract.Terminating as T
import           Data.Abstract.Closure (Closure)
import qualified Data.Abstract.Closure as C
import           Data.Abstract.DiscretePowerset (Pow)
import           Data.Abstract.CallString(CallString)

import           GHC.Exts(IsString(..))
import           GHC.Generics(Generic)
import           Text.Printf

import           Syntax (Expr(..),apply)
import           GenericInterpreter as Generic

type Env = F.Env Text Val
type Cls env = Closure Expr env

-- | Numeric values are approximated with bounded intervals, closure
-- values are approximated with a set of abstract closures.
data Val env = NumVal IV | ClosureVal (Cls env) | TypeError (Pow String) deriving (Eq, Generic, Functor)

-- | Run the abstract interpreter for an interval analysis. The arguments are the
-- maximum interval bound, the depth @k@ of the longest call string,
-- an environment, and the input of the computation.
evalInterval :: (?sensitivity :: Int, ?bound :: IV) => [(Text,Val ())] -> State Label Expr -> Terminating (Error (Pow String) (Val Env))
evalInterval env0 e = snd $
  run (Generic.eval ::
      Fix'
        (ValueT (Val Env)
          (EnvT F.Env Text Val
            (ErrorT (Pow String)
              (TerminatingT
                (FixT _ _
                  (ChaoticT _
                    (StackT Stack _
                      (CacheT (Group Cache) (_,_) _
                        (ContextT (CallString _) _
                          (->)))))))))) Expr (Val Env))
    iterationStrategy
    (T.widening (E.widening W.finite widenVal))
    (F.fromList env0,e0)
  where
    e0 = generate e

    iterationStrategy = Fix.filter apply
                      $ callsiteSensitive @((Expr,Label),Env) ?sensitivity (snd . fst) widenEnv
                      . iterateInner

    widenEnv :: Widening Env
    widenEnv = F.widening (widening (W.bounded ?bound I.widening) S.widening)

    widenVal :: Widening (Val Env)
    widenVal = widening (W.bounded ?bound I.widening) widenEnv

instance (IsString e, ArrowChoice c, ArrowFail e c) => IsVal (Val env) (ValueT (Val env) c) where
  type Join y (ValueT (Val env) c) = ArrowComplete y (ValueT (Val env) c)

  succ = proc x -> case x of
    NumVal n -> returnA -< NumVal $ n + 1 -- uses the `Num` instance of intervals
    _ -> fail -< "Expected a number as argument for 'succ'"

  pred = proc x -> case x of
    NumVal n -> returnA -< NumVal $ n - 1
    _ -> fail -< "Expected a number as argument for 'pred'"

  zero = proc _ -> returnA -< NumVal 0

  if_ f g = proc v -> case v of
    (NumVal (I.Interval i1 i2), (x, y))
      | (i1, i2) == (0, 0) -> f -< x                -- case the interval is exactly zero
      | i1 > 0 || i2 < 0   -> g -< y                -- case the interval does not contain zero
      | otherwise          -> (f -< x) <⊔> (g -< y) -- case the interval contains zero and other numbers.
    _ -> fail -< "Expected a number as condition for 'ifZero'"

instance (IsString e, ArrowChoice c, ArrowFail e c, ArrowClosure Expr (Cls env) c)
    => ArrowClosure Expr (Val env) (ValueT (Val env) c) where
  type Join y (ValueT (Val env) c) = Cls.Join y c
  closure = ValueT $ rmap ClosureVal Cls.closure
  apply (ValueT f) = ValueT $ proc (v,x) -> case v of
    ClosureVal cls -> Cls.apply f -< (cls,x)
    _ -> fail -< "Expected a closure"
  {-# INLINE closure #-}
  {-# INLINE apply #-}

-- instance (IsString e, ArrowChoice c, ArrowFail e c, ArrowLetRec Text (Val env) c) => ArrowLetRec Text (Val env) (ValueT (Val env) c) where
--   letRec (ValueT f) = ValueT $ letRec f
--   {-# INLINE letRec #-}

instance (ArrowChoice c, IsString e, ArrowFail e c, ArrowComplete (Val env) c) => ArrowComplete (Val env) (ValueT (Val env) c) where
  ValueT f <⊔> ValueT g = ValueT $ proc x -> do
    v <- (f -< x) <⊔> (g -< x)
    case v of
      TypeError m -> fail -< fromString (show m)
      _           -> returnA -< v

instance PreOrd env => PreOrd (Val env) where
  _ ⊑ TypeError _ = True
  NumVal n1 ⊑ NumVal n2 = n1 ⊑ n2
  ClosureVal c1 ⊑ ClosureVal c2 = c1 ⊑ c2
  _ ⊑ _ = False

instance Complete env => Complete (Val env) where
  (⊔) = W.toJoin2 widening (⊔) (⊔)

instance PreOrd env => UpperBounded (Val env) where
  top = TypeError (singleton "Value outside the allowed range of the analysis")

-- TODO: Fix widening
widening :: W.Widening IV -> W.Widening env -> W.Widening (Val env)
widening w _ (NumVal x) (NumVal y) = second NumVal (x `w` y)
widening _ wenv (ClosureVal cs) (ClosureVal cs') = second ClosureVal $ C.widening wenv cs cs'
widening _ _ (NumVal _) (ClosureVal _) = (Unstable,TypeError (singleton "cannot unify a number with a closure"))
widening _ _ (ClosureVal _) (NumVal _) = (Unstable,TypeError (singleton "cannot unify a closure with a number"))
widening _ _ (TypeError m1) (TypeError m2) = (Stable,TypeError (m1 <> m2))
widening _ _ _ (TypeError m2) = (Stable,TypeError m2)
widening _ _ (TypeError m1) _ = (Stable,TypeError m1)

instance Hashable env => Hashable (Val env)
instance Show env => Show (Val env) where
  show (NumVal iv) = show iv
  show (ClosureVal cls) = show cls
  show (TypeError m) = printf "TypeError: %s" (show m)

type IV = Interval (InfiniteNumber Int)

instance Traversable Val where
  traverse _ (NumVal n) = pure $ NumVal n
  traverse _ (TypeError e) = pure $ TypeError e
  traverse f (ClosureVal c) = ClosureVal <$> traverse f c

instance Foldable Val where
  foldMap = foldMapDefault
