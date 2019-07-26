{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
-- | k-CFA analysis for PCF where numbers are approximated by intervals.
module IntervalAnalysis where

import           Prelude hiding (Bounded,fail,(.),exp)

import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Trans
import           Control.Arrow.Environment as Env
import           Control.Arrow.Order
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.Environment
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.IterationStrategy
import qualified Control.Arrow.Transformer.Abstract.Fix.IterationStrategy as S
import           Control.Arrow.Transformer.Abstract.Terminating

import           Control.Monad.State hiding (lift,fail)

import qualified Data.HashSet as H
import qualified Data.HashMap.Lazy as M
import           Data.Hashable
import           Data.Label
import           Data.Order
import           Data.Text (Text)
import qualified Data.Lens as L
import           Data.Utils

import           Data.Abstract.StrongMap(Map)
import qualified Data.Abstract.StrongMap as SM
import           Data.Abstract.Error (Error)
import qualified Data.Abstract.Error as E
import           Data.Abstract.InfiniteNumbers
import           Data.Abstract.Interval (Interval)
import qualified Data.Abstract.Interval as I
import qualified Data.Abstract.Widening as W
import qualified Data.Abstract.StackWidening as SW
import           Data.Abstract.Terminating(Terminating)
import qualified Data.Abstract.Terminating as T
import           Data.Abstract.Closure (Closure)
import qualified Data.Abstract.Closure as C
import           Data.Abstract.DiscretePowerset (Pow)
    
import           GHC.Generics(Generic)
import           GHC.Exts(IsString(..))
import           Text.Printf

import           Syntax (Expr(..),apply)
import           GenericInterpreter
import           VariableAnalysis(variables)

type Env = Map Text Val

-- | Numeric values are approximated with bounded intervals, closure
-- values are approximated with a set of abstract closures.
data Val = NumVal IV | ClosureVal (Closure Expr Env) | TypeError (Pow String) deriving (Eq, Generic)

-- | Run the abstract interpreter for an interval analysis. The arguments are the
-- maximum interval bound, the depth @k@ of the longest call string,
-- an environment, and the input of the computation.
evalInterval :: (?bound :: IV) => Int -> [(Text,Val)] -> State Label Expr -> Terminating (Error (Pow String) Val)
evalInterval k env0 e = snd $
    run (eval :: Fix Expr Val
                (ValueT Val
                  (EnvT Text Val
                    (ErrorT (Pow String)
                      (TerminatingT
                        (FixT _ _
                          (StackWideningT _ _
                            (ChaoticT _ _ (->)))))))) Expr Val)
    iterationStrategy
    (SM.fromList env0,e0)
  where
    e0 = generate e
    vars = variables e0
    
    iterationStrategy = S.filter apply
                      $ tightenEnv
                      $ S.stackWidening stackWiden
                      $ S.chaotic (T.widening (E.widening W.finite widenVal))

    stackWiden = SW.groupBy (L.iso' (\(env,exp) -> (exp,env)) (\(exp,env) -> (env,exp)))
               $ SW.reuseFirst
               $ SW.maxSize k
               $ SW.fromWidening (SM.widening widenVal)
               $ SW.finite

    widenVal = widening (W.bounded ?bound I.widening)

    tightenEnv strat f = proc (env,(exp,lab)) -> do
      case M.lookup exp vars of
        Just (scope,used) -> strat f -< (SM.delete (scope `H.difference` used) env,(exp,lab))
        Nothing -> strat f -< (env,(exp,lab))

instance (IsString e, ArrowChoice c, ArrowFail e c) => IsNum Val (ValueT Val c) where
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

instance (IsString e, ArrowChoice c, ArrowFail e c, ArrowLowerBounded c, ArrowComplete Val (ValueT Val c), ArrowClosure var Val Env c)
    => IsClosure Val (ValueT Val c) where
  closure _ = proc e -> do
    env <- Env.ask -< ()
    returnA -< ClosureVal (C.closure e env)
  applyClosure f = proc (fun, arg) -> case fun of
    ClosureVal cls -> (| C.apply (\(e,env) -> Env.local f -< (env,(e,arg))) |) cls
    _ -> fail -< "Expected a closure"

instance (ArrowChoice c, IsString e, ArrowFail e c, ArrowComplete Val c) => ArrowComplete Val (ValueT Val c) where
  ValueT f <⊔> ValueT g = ValueT $ proc x -> do
    v <- ((f -< x) <⊔> (g -< x))
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
widening _ (NumVal _) (ClosureVal _) = (W.Instable,TypeError (singleton "cannot unify a number with a closure"))
widening _ (ClosureVal _) (NumVal _) = (W.Instable,TypeError (singleton "cannot unify a closure with a number"))
widening _ (TypeError m1) (TypeError m2) = (W.Stable,TypeError (m1 <> m2))
widening _ _ (TypeError m2) = (W.Stable,TypeError m2)
widening _ (TypeError m1) _ = (W.Stable,TypeError m1)

-- instance HasLabel (F.Map Text Addr Val,Expr) Label where
--   label (_,e) = label e

instance Hashable Val
instance Show Val where
  show (NumVal iv) = show iv
  show (ClosureVal cls) = show cls
  show (TypeError m) = printf "TypeError: %s" (show m)

type IV = Interval (InfiniteNumber Int)
