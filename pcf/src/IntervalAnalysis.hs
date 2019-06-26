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

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Alloc
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Trans
import           Control.Arrow.Conditional as Cond
import           Control.Arrow.Environment
import           Control.Arrow.Abstract.Join
import           Control.Arrow.Transformer.Abstract.Contour
import           Control.Arrow.Transformer.Abstract.BoundedEnvironment
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Terminating
import           Control.Monad.State hiding (lift,fail)

import           Data.Hashable
import           Data.Label
import           Data.Order
import           Data.Text (Text)
import           Data.Profunctor
import qualified Data.Lens as L

import qualified Data.Abstract.FiniteMap as F
import           Data.Abstract.Error (Error)
import qualified Data.Abstract.Error as E
import           Data.Abstract.InfiniteNumbers
import           Data.Abstract.Interval (Interval)
import qualified Data.Abstract.Interval as I
import qualified Data.Abstract.Widening as W
import           Data.Abstract.StackWidening(StackWidening)
import qualified Data.Abstract.StackWidening as SW
import           Data.Abstract.Terminating(Terminating)
import qualified Data.Abstract.Terminating as T
import           Data.Abstract.Closure (Closure)
import qualified Data.Abstract.Closure as C
import           Data.Abstract.DiscretePowerset (Pow)
import           Data.Abstract.IterationStrategy(IterationStrategy)
import qualified Data.Abstract.IterationStrategy as S
    
import           GHC.Generics(Generic)
import           GHC.Exts(IsString(..))

import           Syntax (Expr(..),apply)
import           GenericInterpreter

type Env = F.Map Text (Text, CallString Label) Val

-- | Numeric values are approximated with bounded intervals, closure
-- values are approximated with a set of abstract closures.
data Val = NumVal IV | ClosureVal (Closure Expr Env) | Top deriving (Eq, Generic)

-- | Addresses for this analysis are variables paired with the k-bounded call string.
type Addr = (Text,CallString Label)

-- | Run the abstract interpreter for the k-CFA / Interval analysis. The arguments are the
-- maximum interval bound, the depth @k@ of the longest call string,
-- an environment, and the input of the computation.
evalInterval :: (?bound :: IV) => Int -> [(Text,Val)] -> State Label Expr -> Terminating (Error (Pow String) Val)
evalInterval k env0 e =
  runFixT iterationStrategy
    (runTerminatingT
      (runErrorT
        (runContourT k
          (runEnvT alloc
            (runIntervalT
              (eval ::
                Fix Expr Val
                  (IntervalT
                    (EnvT Text Addr Val
                      (ContourT Label
                        (ErrorT (Pow String)
                          (TerminatingT
                            (FixT _ _ () () (->))))))) Expr Val))))))
    (env0,generate e)
  where
    iterationStrategy :: IterationStrategy _ _ (Env,Expr) _
    iterationStrategy = S.filter apply
                      $ S.chaotic stackWiden (T.widening (E.widening W.finite widenVal))

    stackWiden :: StackWidening _ (Env,(Expr,Label))
    stackWiden = SW.groupBy (L.iso' (\(env,exp) -> (exp,env)) (\(exp,env) -> (env,exp)))
               $ SW.reuseFirst
               $ SW.maxSize 3
               $ SW.fromWidening (F.widening widenVal)
               $ SW.finite

    widenVal = widening (W.bounded ?bound I.widening)

newtype IntervalT c x y = IntervalT { runIntervalT :: c x y } deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowFail e,ArrowJoin)
type instance Fix x y (IntervalT c) = IntervalT (Fix x y c)
deriving instance ArrowFix x y c => ArrowFix x y (IntervalT c)
deriving instance ArrowEnv var val env c => ArrowEnv var val env (IntervalT c)

instance ArrowTrans IntervalT where
  type Dom IntervalT x y = x
  type Cod IntervalT x y = y
  lift = IntervalT
  unlift = runIntervalT

instance (IsString e, ArrowChoice c, ArrowFail e c, ArrowJoin c) => IsVal Val (IntervalT c) where
  succ = proc x -> case x of
    Top -> (returnA -< NumVal top) <⊔> (fail -< "Expected a number as argument for 'succ'")
    NumVal n -> returnA -< NumVal $ n + 1 -- uses the `Num` instance of intervals
    ClosureVal _ -> fail -< "Expected a number as argument for 'succ'"
  pred = proc x -> case x of
    Top -> (returnA -< NumVal top) <⊔> (fail -< "Expected a number as argument for 'pred'")
    NumVal n -> returnA -< NumVal $ n - 1
    ClosureVal _ -> fail -< "Expected a number as argument for 'pred'"
  zero = proc _ -> returnA -< (NumVal 0)

instance (IsString e, ArrowChoice c, ArrowJoin c, ArrowFail e c) => ArrowCond Val (IntervalT c) where
  type Join (IntervalT c) x y = Complete y
  if_ f g = proc v -> case v of
    (Top, (x,y)) -> (f -< x) <⊔> (g -< y) <⊔> (fail -< "Expected a number as condition for 'ifZero'")
    (NumVal (I.Interval i1 i2), (x, y))
      | (i1, i2) == (0, 0) -> f -< x                -- case the interval is exactly zero
      | i1 > 0 || i2 < 0   -> g -< y                -- case the interval does not contain zero
      | otherwise          -> (f -< x) <⊔> (g -< y) -- case the interval contains zero and other numbers.
    (ClosureVal _, _)      -> fail -< "Expected a number as condition for 'ifZero'"

instance (IsString e, ArrowChoice c, ArrowFail e c, ArrowJoin c)
    => IsClosure Val (F.Map Text Addr Val) (IntervalT c) where
  closure = arr $ \(e, env) -> ClosureVal (C.closure e env)
  applyClosure f = proc (fun, arg) -> case fun of
    Top -> (returnA -< Top) <⊔> (fail -< "Expected a closure")
    ClosureVal cls -> (| C.apply (\(e,env) -> f -< ((e,env),arg)) |) cls
    NumVal _ -> fail -< "Expected a closure"

instance PreOrd Val where
  _ ⊑ Top = True
  NumVal n1 ⊑ NumVal n2 = n1 ⊑ n2
  ClosureVal c1 ⊑ ClosureVal c2 = c1 ⊑ c2
  _ ⊑ _ = False

instance Complete Val where
  (⊔) = W.toJoin widening (⊔)

widening :: W.Widening IV -> W.Widening Val
widening w (NumVal x) (NumVal y) = second NumVal (x `w` y)
widening w (ClosureVal cs) (ClosureVal cs') =
  second ClosureVal $ C.widening (F.widening (widening w)) cs cs'
widening _ Top Top = (W.Stable,Top)
widening _ _ _ = (W.Instable,Top)

instance UpperBounded Val where
  top = Top

instance HasLabel (F.Map Text Addr Val,Expr) Label where
  label (_,e) = label e

instance Hashable Val
instance Show Val where
  show (NumVal iv) = show iv
  show (ClosureVal cls) = show cls
  show Top = "⊤"

type IV = Interval (InfiniteNumber Int)
