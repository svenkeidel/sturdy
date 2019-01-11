{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | k-CFA analysis for PCF where numbers are approximated by intervals.
module IntervalAnalysis where

import           Prelude hiding (Bounded,fail)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Alloc
import           Control.Arrow.Fail
import           Control.Arrow.Const
import           Control.Arrow.Fix
import           Control.Arrow.Conditional
import           Control.Arrow.Environment
import           Control.Arrow.Transformer.Abstract.Contour
import           Control.Arrow.Transformer.Abstract.BoundedEnvironment
import           Control.Arrow.Transformer.Abstract.Failure
import           Control.Arrow.Transformer.Abstract.Fixpoint
import           Control.Arrow.Transformer.Const
import           Control.Monad.State hiding (lift,fail)

import           Data.Foldable (toList)
import           Data.Hashable
import           Data.Label
import           Data.Order
import           Data.Text (Text)

import           Data.Abstract.Powerset(Pow)
import           Data.Abstract.PreciseStore(Store)
import qualified Data.Abstract.PreciseStore as S
import qualified Data.Abstract.FiniteMap as F
import           Data.Abstract.Failure (Error)
import qualified Data.Abstract.Failure as E
import           Data.Abstract.Environment (Env)
import qualified Data.Abstract.Environment as M
import           Data.Abstract.InfiniteNumbers
import           Data.Abstract.Interval (Interval)
import qualified Data.Abstract.Interval as I
import qualified Data.Abstract.Widening as W
import qualified Data.Abstract.StackWidening as SW
import           Data.Abstract.Terminating(Terminating)
    
import           GHC.Generics

import           Syntax (Expr)
import           SharedSemantics

-- | Abstract closures are expressions paired with an abstract
-- environment, consisting of a mapping from variables to addresses
-- and a mapping from addresses to stores.
newtype Closure = Closure (Store Expr (F.Map Text Addr Val)) deriving (Eq,Generic,PreOrd,Show)

-- | Numeric values are approximated with bounded intervals, closure
-- values are approximated with a set of abstract closures.
data Val = NumVal IV | ClosureVal Closure | Top deriving (Eq, Generic)

-- | Addresses for this analysis are variables paired with the k-bounded call string.
type Addr = (Text,CallString Label)

-- | Interpreter arrow for the k-CFA / interval analysis.
newtype Interp s x y =
  Interp (
    Fix Expr Val                  -- type of the fixpoint cache
      (EnvT Text Addr Val         -- threads the environment and store
        (ContourT Label           -- records the k-bounded call stack used for address allocation
          (FailureT String        -- allows to fail with an error message
            (FixT s () ()
              (->))))) x y)

widening :: W.Widening IV -> W.Widening Val
widening w _ Top = Top
widening w (NumVal x) (NumVal y) = NumVal (x `w` y)
widening w (ClosureVal (Closure cs)) (ClosureVal (Closure cs')) = ClosureVal $ Closure $ S.widening _ cs cs'


-- | Run an interpreter computation on inputs. The arguments are the
-- maximum interval bound, the depth `k` of the longest call string,
-- an environment, and the input of the computation.
runInterp :: Interp SW.Unit x y -> IV -> Int -> [(Text,Val)] -> x -> Terminating (Error String y)
runInterp (Interp f) b k env x = 
  runFixT' _ (E.widening (widening _))
    (runFailureT
      (runContourT k
        (runEnvT alloc
          f)))
    (env,x)

-- | The top-level interpreter functions that executes the analysis.
evalInterval :: (?bound :: IV) => Int -> [(Text,Val)] -> State Label Expr -> Terminating (Error String Val)
evalInterval k env e = runInterp eval ?bound k env (generate e)

instance IsVal Val (Interp s) where
  succ = proc x -> case x of
    Top -> returnA -< Top
    NumVal n -> returnA -< NumVal $ n + 1 -- uses the `Num` instance of intervals
    ClosureVal _ -> fail -< "Expected a number as argument for 'succ'"
  pred = proc x -> case x of
    Top -> returnA -< Top
    NumVal n -> returnA -< NumVal $ n + negate 1
    ClosureVal _ -> fail -< "Expected a number as argument for 'pred'"
  zero = proc _ -> returnA -< (NumVal 0)

instance (Complete z, UpperBounded z) => ArrowCond Val x y z (Interp s) where
  if_ f g = proc v -> case v of
    (Top, _) -> returnA -< top
    (NumVal (I.Interval i1 i2), (x, y))
      | (i1, i2) == (0, 0) -> f -< x      -- case the interval is exactly zero
      | i1 > 0 || i2 < 0 -> g -< y        -- case the interval does not contain zero
      | otherwise -> (f -< x) ⊔ (g -< y)  -- case the interval contains zero and other numbers.
    (ClosureVal _, _) -> fail -< "Expected a number as condition for 'ifZero'"

instance IsClosure Val (F.Map Text Addr Val) (Interp s) where
  closure = arr $ \(e, env) -> ClosureVal (return (Closure e env))
  applyClosure f = proc (fun, arg) -> case fun of
    Top -> returnA -< Top
    ClosureVal cls ->
      -- Apply the interpreter function `f` on all closures and join their results.
      lubA (proc (Closure e env,arg) -> f -< ((e,env),arg)) -< [ (c,arg) | c <- toList cls]
    NumVal _ -> fail -< "Expected a closure"

deriving instance Category (Interp s)
deriving instance Arrow (Interp s)
deriving instance ArrowChoice (Interp s)
deriving instance ArrowFail String (Interp s)
deriving instance ArrowEnv Text Val (F.Map Text Addr Val) (Interp s)
deriving instance ArrowFix Expr Val (Interp s)
deriving instance PreOrd y => PreOrd (Interp s x y)
deriving instance Complete y => Complete (Interp s x y)
deriving instance PreOrd y => LowerBounded (Interp s x y)

instance PreOrd Val where
  _ ⊑ Top = True
  NumVal n1 ⊑ NumVal n2 = n1 ⊑ n2
  ClosureVal c1 ⊑ ClosureVal c2 = c1 ⊑ c2
  _ ⊑ _ = False

instance Complete Val where
  Top ⊔ _ = Top
  _ ⊔ Top = Top
  NumVal x ⊔ NumVal y = NumVal (x ⊔ y)
  ClosureVal x ⊔ ClosureVal y = ClosureVal (x ⊔ y)
  _ ⊔ _ = Top

-- instance Widening Val where
--   -- Only intervals require widening, everything else has finite height.
--   NumVal x ▽ NumVal y = NumVal (x ▽ y)
--   x ▽ y =  x ⊔ y

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
