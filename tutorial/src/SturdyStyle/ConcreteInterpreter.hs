{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImplicitParams #-}
-- | This file instantiates the generic interpreter with concrete
-- values to implement the concrete interpreter.  All we have to do is
-- to implement the `IsValue` interface, everything else is provided
-- by the sturdy standard library.
module SturdyStyle.ConcreteInterpreter where

import           Prelude hiding (lookup,and,fail)

import           Control.Arrow
import           Control.Arrow.Fail as Fail
import qualified Control.Arrow.Trans as Trans
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Concrete.Failure
import           Control.Arrow.Transformer.Concrete.Environment
import           Control.Arrow.Transformer.Concrete.Store

import           Data.Concrete.Error (Error)
import           Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Profunctor
import           Data.Label
import qualified Data.Function as Function

import           SturdyStyle.GenericInterpreter(IsValue,ArrowAlloc)
import qualified SturdyStyle.GenericInterpreter as Generic
import           Syntax

data Val = BoolVal Bool | NumVal Integer deriving (Eq,Show)
type Addr = Label
type Env = HashMap String Addr
type Store = HashMap Addr Val

instance (ArrowChoice c, ArrowFail String c, Fail.Join Val c) => IsValue Val (ValueT Val c) where
  type JoinVal y (ValueT Val c) = (Fail.Join y c)

  numLit = proc n -> returnA -< NumVal $ toInteger n
  add = proc (v1,v2) -> case (v1,v2) of
      (NumVal n1, NumVal n2) -> returnA -< NumVal (n1 + n2)
      (_,_) -> fail -< "Expected two numbers as arguments for +"

  lt = proc (v1,v2) -> case (v1,v2) of
      (NumVal n1, NumVal n2) -> returnA -< BoolVal (n1 < n2)
      (_,_) -> fail -< "Expected two booleans as arguments for <"


  boolLit = proc b -> returnA -< BoolVal b
  and = proc (v1,v2) -> case (v1,v2) of
      (BoolVal b1, BoolVal b2) -> returnA -< BoolVal (b1 && b2)
      (_,_) -> fail -< "Expected two booleans as arguments for &&"

  if_ f g = proc (v,(x,y)) -> case v of
    BoolVal True -> f -< x
    BoolVal False -> g -< y
    _ -> fail -< "Expected a boolean expression as condition for an if"

instance (ArrowChoice c, Profunctor c) => ArrowAlloc Addr (ValueT Val c) where
  alloc = proc (_,_,l) -> returnA -< l

run :: Env -> Store -> [Statement] -> Error String (HashMap Addr Val)
run initEnv initStore prog =
  let ?fixpointAlgorithm = Function.fix in
  fst <$>
    Trans.run
      (Generic.run ::
        ValueT Val
          (EnvT Env
            (StoreT Store
              (FailureT String
                (->)))) [Statement] ())
      (initStore,(initEnv,prog))

runWithInitVals :: [(String,Val)] -> [LStatement]
                     -> (Error String (HashMap Addr Val), Env)
runWithInitVals initVals stmts =
  (run initEnv initStore prog, initEnv)
  where
    (prog, start) = generateState (sequence stmts)
    labels = [start..]
    (strings,vals) = unzip initVals
    initEnv = M.fromList $ zipWith (,) strings labels
    initStore = M.fromList $ zipWith (,) labels vals

