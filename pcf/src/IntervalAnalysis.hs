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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-partial-type-signatures #-}
-- | k-CFA analysis for PCF where numbers are approximated by intervals.
module IntervalAnalysis where

import           Prelude hiding (Bounded,fail)

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
import           Control.Arrow.Transformer.Abstract.Failure
import           Control.Arrow.Transformer.Abstract.Fixpoint
import           Control.Monad.State hiding (lift,fail)

import           Data.Hashable
import           Data.Label
import           Data.Order
import           Data.Monoidal(Iso(..))
import           Data.Text (Text)
import           Data.Profunctor

import           Data.Abstract.Map(Map)
import qualified Data.Abstract.Map as M
import qualified Data.Abstract.FiniteMap as F
import           Data.Abstract.Failure (Failure)
import qualified Data.Abstract.Failure as E
import           Data.Abstract.InfiniteNumbers
import           Data.Abstract.Interval (Interval)
import qualified Data.Abstract.Interval as I
import qualified Data.Abstract.Widening as W
import qualified Data.Abstract.StackWidening as SW
import           Data.Abstract.Terminating(Terminating)
    
import           GHC.Generics(Generic)
import           GHC.Exts(toList)

import           Syntax (Expr)
import           GenericInterpreter

-- | Abstract closures are expressions paired with an abstract
-- environment, consisting of a mapping from variables to addresses
-- and a mapping from addresses to stores.
newtype Closure = Closure (Map Expr (F.Map Text Addr Val)) deriving (Eq,Generic,PreOrd,Complete,Show)

-- | Numeric values are approximated with bounded intervals, closure
-- values are approximated with a set of abstract closures.
data Val = NumVal IV | ClosureVal Closure | Top deriving (Eq, Generic)

-- | Addresses for this analysis are variables paired with the k-bounded call string.
type Addr = (Text,CallString Label)

-- | Run the abstract interpreter for the k-CFA / Interval analysis. The arguments are the
-- maximum interval bound, the depth @k@ of the longest call string,
-- an environment, and the input of the computation.
evalInterval :: (?bound :: IV) => Int -> [(Text,Val)] -> State Label Expr -> Terminating (Failure String Val)
evalInterval k env e = -- runInterp eval ?bound k env (generate e)
  runFixT' stackWiden (E.widening widenVal)
    (runFailureT
      (runContourT k
        (runEnvT alloc
          (runIntervalT
            (eval ::
              Fix Expr Val
                (IntervalT
                  (EnvT Text Addr Val
                    (ContourT Label
                      (FailureT String
                        (FixT _ () () (->)))))) Expr Val)))))
    (env,generate e)
  where
    widenVal = widening (W.bounded ?bound top)
    stackWiden = SW.categorize (Iso (\(ev,ex) -> (ex,ev)) (\(ex,ev) -> (ev,ex)))
               $ SW.stack
               $ SW.maxSize 3
               $ SW.reuse (\_ l -> head l)
               $ SW.fromWidening (F.widening widenVal)

newtype IntervalT c x y = IntervalT { runIntervalT :: c x y } deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowFail e,ArrowJoin)
type instance Fix x y (IntervalT c) = IntervalT (Fix x y c)
deriving instance ArrowFix x y c => ArrowFix x y (IntervalT c)
deriving instance ArrowEnv var val env c => ArrowEnv var val env (IntervalT c)

instance ArrowTrans IntervalT where
  type Dom IntervalT x y = x
  type Cod IntervalT x y = y
  lift = IntervalT
  unlift = runIntervalT

instance (ArrowChoice c, ArrowFail String c, ArrowJoin c) => IsVal Val (IntervalT c) where
  succ = proc x -> case x of
    Top -> (returnA -< NumVal top) <⊔> (fail -< "Expected a number as argument for 'succ'")
    NumVal n -> returnA -< NumVal $ n + 1 -- uses the `Num` instance of intervals
    ClosureVal _ -> fail -< "Expected a number as argument for 'succ'"
  pred = proc x -> case x of
    Top -> (returnA -< NumVal top) <⊔> (fail -< "Expected a number as argument for 'pred'")
    NumVal n -> returnA -< NumVal $ n - 1
    ClosureVal _ -> fail -< "Expected a number as argument for 'pred'"
  zero = proc _ -> returnA -< (NumVal 0)

instance (ArrowChoice c, ArrowJoin c, ArrowFail String c) => ArrowCond Val (IntervalT c) where
  type Join (IntervalT c) x y = Complete y
  if_ f g = proc v -> case v of
    (Top, (x,y)) -> (f -< x) <⊔> (g -< y) <⊔> (fail -< "Expected a number as condition for 'ifZero'")
    (NumVal (I.Interval i1 i2), (x, y))
      | (i1, i2) == (0, 0) -> f -< x                -- case the interval is exactly zero
      | i1 > 0 || i2 < 0   -> g -< y                -- case the interval does not contain zero
      | otherwise          -> (f -< x) <⊔> (g -< y) -- case the interval contains zero and other numbers.
    (ClosureVal _, _)      -> fail -< "Expected a number as condition for 'ifZero'"

instance (ArrowChoice c, ArrowFail String c, ArrowJoin c)
    => IsClosure Val (F.Map Text Addr Val) (IntervalT c) where
  closure = arr $ \(e, env) -> ClosureVal (Closure [(e,env)])
  applyClosure f = proc (fun, arg) -> case fun of
    Top -> returnA -< Top
    ClosureVal (Closure cls) ->
      -- Apply the interpreter function `f` on all closures and join their results.
      (| joinList (returnA -< Top) (\(e,env) -> f -< ((e,env),arg)) |)
         (toList cls)
    NumVal _ -> fail -< "Expected a closure"

instance PreOrd Val where
  _ ⊑ Top = True
  NumVal n1 ⊑ NumVal n2 = n1 ⊑ n2
  ClosureVal c1 ⊑ ClosureVal c2 = c1 ⊑ c2
  _ ⊑ _ = False

instance Complete Val where
  (⊔) = widening (⊔)

widening :: W.Widening IV -> W.Widening Val
widening w (NumVal x) (NumVal y) = NumVal (x `w` y)
widening w (ClosureVal (Closure cs)) (ClosureVal (Closure cs')) =
  ClosureVal $ Closure $ M.widening (F.widening (widening w)) cs cs'
widening _ _ _ = Top

instance UpperBounded Val where
  top = Top

instance HasLabel (F.Map Text Addr Val,Expr) Label where
  label (_,e) = label e

instance Hashable Closure
instance Hashable Val
instance Show Val where
  show (NumVal iv) = show iv
  show (ClosureVal cls) = show cls
  show Top = "⊤"

type IV = Interval (InfiniteNumber Int)
