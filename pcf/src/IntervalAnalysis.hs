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
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Trans
import           Control.Arrow.Conditional as Cond
import           Control.Arrow.Environment
import           Control.Arrow.Abstract.Join
import           Control.Arrow.Transformer.Abstract.Environment
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

import           Data.Abstract.WeakMap(Map)
import qualified Data.Abstract.WeakMap as M
import           Data.Abstract.Error (Error)
import qualified Data.Abstract.Error as E
import           Data.Abstract.InfiniteNumbers
import           Data.Abstract.Interval (Interval)
import qualified Data.Abstract.Interval as I
import qualified Data.Abstract.Widening as W
import qualified Data.Abstract.StackWidening as SW
import           Data.Abstract.Terminating(Terminating)
import qualified Data.Abstract.Terminating as T
import           Data.Abstract.DiscretePowerset(Pow)
import           Data.Abstract.Closure(Closure)
import qualified Data.Abstract.Closure as C

import           GHC.Generics(Generic)
import           GHC.Exts(IsString(..))

import           Syntax (Expr(..))
import           GenericInterpreter

type Env = Map Text Val
data Val = NumVal IV | ClosureVal (Closure Expr Env) | Top deriving (Eq, Generic)

instance PreOrd Val where
  _ ⊑ Top = True
  NumVal n1 ⊑ NumVal n2 = n1 ⊑ n2
  ClosureVal c1 ⊑ ClosureVal c2 = c1 ⊑ c2
  _ ⊑ _ = False

instance Complete Val where
  (⊔) = W.toJoin widening (⊔)

widening :: W.Widening IV -> W.Widening Val
widening w (NumVal x) (NumVal y) = second NumVal (x `w` y)
widening w (ClosureVal cs) (ClosureVal cs') = second ClosureVal $ C.widening (M.widening (widening w)) cs cs'
widening _ Top Top = (W.Stable,Top)
widening _ _ _ = (W.Instable,Top)

instance UpperBounded Val where
  top = Top

-- | The abstract interpreter for Interval analysis.
evalInterval :: (?bound :: IV) => Int -> [(Text,Val)] -> State Label Expr -> Terminating (Error (Pow String) Val)
evalInterval k env0 e =
  runFixT stackWiden (T.widening (E.widening W.finite widenVal))
    (runTerminatingT
      (runErrorT
        (runEnvT
          (runIntervalT
            (eval ::
              Fix Expr Val
                (IntervalT
                  (EnvT Text Val
                    (ErrorT (Pow String)
                      (TerminatingT
                        (FixT _ () () (->)))))) Expr Val)))))
    (M.fromList env0,generate e)
  where
    stackWiden :: SW.StackWidening _ (Env,Expr)
    stackWiden = SW.filter (\(_,ex) -> case ex of Apply {} -> True; _ -> False)
               $ SW.groupBy (L.iso' (\(env,exp) -> (exp,env)) (\(exp,env) -> (env,exp)))
               $ SW.stack
               $ SW.reuseFirst
               $ SW.maxSize k
               $ SW.fromWidening (M.widening widenVal)

    widenVal = widening (W.bounded ?bound I.widening)

newtype IntervalT c x y = IntervalT { runIntervalT :: c x y } deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowFail e,ArrowJoin)

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
    => IsClosure Val (Map Text Val) (IntervalT c) where
  closure = arr $ \(e, env) -> ClosureVal (C.closure e env)
  applyClosure f = proc (fun, arg) -> case fun of
    Top -> (returnA -< Top) <⊔> (fail -< "Expected a closure")
    ClosureVal cls -> (| C.apply (\(e,env) -> f -< ((e,env),arg)) |) cls
    NumVal _ -> fail -< "Expected a closure"


instance Hashable Val
instance Show Val where
  show (NumVal iv) = show iv
  show (ClosureVal cls) = show cls
  show Top = "⊤"

type IV = Interval (InfiniteNumber Int)

type instance Fix x y (IntervalT c) = IntervalT (Fix x y c)
deriving instance ArrowFix x y c => ArrowFix x y (IntervalT c)
deriving instance ArrowEnv var val env c => ArrowEnv var val env (IntervalT c)
instance ArrowTrans IntervalT where
  type Dom IntervalT x y = x
  type Cod IntervalT x y = y
  lift = IntervalT
  unlift = runIntervalT
