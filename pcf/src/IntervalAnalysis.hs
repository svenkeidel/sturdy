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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-partial-type-signatures #-}
-- | k-CFA analysis for PCF where numbers are approximated by intervals.
module IntervalAnalysis where

import           Prelude hiding (Bounded,fail,(.),exp,filter)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Arrow.Environment(extend')
import           Control.Arrow.Fix
import           Control.Arrow.Fix.Parallel(parallel)
import qualified Control.Arrow.Fix.Context as Ctx
import           Control.Arrow.Trans
import           Control.Arrow.Closure (ArrowClosure,IsClosure(..))
import qualified Control.Arrow.Closure as Cls
import           Control.Arrow.Order
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.FiniteEnvironment
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Context
import           Control.Arrow.Transformer.Abstract.Fix.Stack
-- import           Control.Arrow.Transformer.Abstract.Fix.Trace
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable(CacheT,Cache,Parallel,Monotone,type (**),Group)
import           Control.Arrow.Transformer.Abstract.Terminating

import           Control.Monad.State hiding (lift,fail)

import           Data.Identifiable
import           Data.Hashable
import           Data.Label
import           Data.Order
import           Data.Text (Text)
import           Data.Utils
import           Data.Profunctor
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map
import           Data.HashSet(HashSet)

import           Data.Abstract.Error (Error)
import qualified Data.Abstract.Error as E
import           Data.Abstract.InfiniteNumbers
import           Data.Abstract.Interval (Interval)
import qualified Data.Abstract.Interval as I
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

type Cls = Closure Expr (HashSet (HashMap Text Addr))
type Addr = (Text,Ctx)
type Env = HashMap Text Addr
type Store = HashMap Addr Val
type Ctx = CallString Label

-- | Numeric values are approximated with bounded intervals, closure
-- values are approximated with a set of abstract closures.
data Val = NumVal IV | ClosureVal Cls | TypeError (Pow String) deriving (Eq, Generic)

-- Input and output type of the fixpoint.
type In  = (Store, ((Expr,Label),Env))
type Out = (Store, Terminating (Error (Pow String) Val))

-- | Run the abstract interpreter for an interval analysis. The arguments are the
-- maximum interval bound, the depth @k@ of the longest call string,
-- an environment, and the input of the computation.
evalInterval :: (?sensitivity :: Int, ?bound :: Interval Int) => [(Text,Val)] -> State Label Expr -> (Store, Terminating (Error (Pow String) Val))
evalInterval env0 e = snd $
  run (extend' (Generic.eval ::
      Fix'
        (ValueT Val
          (ErrorT (Pow String)
            (TerminatingT
              (EnvT Text Addr Val
                (FixT _ _
                  (-- ChaoticT In
                    (-- TraceT
                      (StackT Stack In
                        (CacheT (Monotone ** Parallel (Group Cache)) In Out
                          (ContextT Ctx
                            (->))))))))))) Expr Val))
    (alloc, widenVal)
    iterationStrategy
    (widenStore widenVal, T.widening (E.widening W.finite widenVal))
    (Map.empty,(Map.empty,(env0,e0)))
  where
    e0 = generate e

    alloc = proc (var,_) -> do
      ctx <- Ctx.askContext @Ctx -< ()
      returnA -< (var,ctx)

    iterationStrategy =
      traceShow .
      -- traceCache show .
      Ctx.recordCallsite ?sensitivity (\(_,(_,expr)) -> case expr of App _ _ l -> Just l; _ -> Nothing) .
      filter apply parallel

    widenVal :: Widening Val
    widenVal = widening (I.bounded ?bound)

evalInterval' :: (?sensitivity :: Int, ?bound :: Interval Int) => [(Text,Val)] -> State Label Expr -> Terminating (Error (Pow String) Val)
evalInterval' env expr = snd $ evalInterval env expr
{-# INLINE evalInterval' #-}

instance (IsString e, ArrowChoice c, ArrowFail e c) => IsVal Val (ValueT Val c) where
  type Join y (ValueT Val c) = ArrowComplete y (ValueT Val c)

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

instance (IsString e, ArrowChoice c, ArrowFail e c, ArrowClosure Expr Cls c)
    => ArrowClosure Expr Val (ValueT Val c) where
  type Join y Val (ValueT Val c) = Cls.Join y Cls c
  closure = ValueT $ rmap ClosureVal Cls.closure
  apply (ValueT f) = ValueT $ proc (v,x) -> case v of
    ClosureVal cls -> Cls.apply f -< (cls,x)
    _ -> fail -< "Expected a closure"
  {-# INLINE closure #-}
  {-# INLINE apply #-}

-- instance (IsString e, ArrowChoice c, ArrowFail e c, ArrowLetRec Text (Val env) c) => ArrowLetRec Text (Val env) (ValueT (Val env) c) where
--   letRec (ValueT f) = ValueT $ letRec f
--   {-# INLINE letRec #-}

instance (ArrowChoice c, IsString e, ArrowFail e c, ArrowComplete Val c) => ArrowComplete Val (ValueT Val c) where
  ValueT f <⊔> ValueT g = ValueT $ proc x -> do
    v <- (f -< x) <⊔> (g -< x)
    case v of
      TypeError m -> fail -< fromString (show m)
      _           -> returnA -< v

instance PreOrd Val where
  _ ⊑ TypeError _ = True
  NumVal n1 ⊑ NumVal n2 = n1 ⊑ n2
  ClosureVal c1 ⊑ ClosureVal c2 = c1 ⊑ c2
  _ ⊑ _ = False

instance Complete Val where
  (⊔) = W.toJoin widening (⊔)

instance UpperBounded Val where
  top = TypeError (singleton "Value outside the allowed range of the analysis")

-- TODO: Fix widening
widening :: W.Widening IV -> W.Widening Val
widening w (NumVal x) (NumVal y) = second NumVal (x `w` y)
widening _ (ClosureVal cs) (ClosureVal cs') = second ClosureVal $ C.widening W.finite cs cs'
widening _ (NumVal _) (ClosureVal _) = (Unstable,TypeError (singleton "cannot unify a number with a closure"))
widening _ (ClosureVal _) (NumVal _) = (Unstable,TypeError (singleton "cannot unify a closure with a number"))
widening _ (TypeError m1) (TypeError m2) = (Stable,TypeError (m1 <> m2))
widening _ _ (TypeError m2) = (Stable,TypeError m2)
widening _ (TypeError m1) _ = (Stable,TypeError m1)

widenStore :: Identifiable addr => Widening val -> Widening (HashMap addr val)
widenStore w m1 m2
  | Map.keys m1 == Map.keys m2 = sequenceA $ Map.intersectionWith w m1 m2
  | otherwise = (Unstable,Map.unionWith (\x y -> snd (w x y)) m1 m2)

instance Hashable Val
instance Show Val where
  show (NumVal iv) = show iv
  show (ClosureVal cls) = show cls
  show (TypeError m) = printf "TypeError: %s" (show m)

type IV = Interval (InfiniteNumber Int)

instance IsClosure Val (HashSet Env) where
  mapEnvironment f (ClosureVal c) = ClosureVal (mapEnvironment f c)
  mapEnvironment _ v = v
  traverseEnvironment f (ClosureVal c) = ClosureVal <$> traverseEnvironment f c
  traverseEnvironment _ v = pure v

-- instance Traversable Val where
--   traverse _ (NumVal n) = pure $ NumVal n
--   traverse _ (TypeError e) = pure $ TypeError e
--   traverse f (ClosureVal c) = ClosureVal <$> traverse f c

-- instance Foldable Val where
--   foldMap = foldMapDefault
