{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module ConcreteInterpreter where

import           Prelude hiding (read,fail,(.))
import qualified Prelude as P

import           Syntax
import           GenericInterpreter
import qualified GenericInterpreter as Generic

import           Data.Concrete.Failure (Failure)
import           Data.Hashable
import           Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Text (Text)
import           Data.Label
import           Data.Profunctor

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Environment
import           Control.Arrow.Store
import           Control.Arrow.Alloc
import           Control.Arrow.Conditional as Cond
import           Control.Arrow.Random
import           Control.Arrow.Transformer.Concrete.Failure
import           Control.Arrow.Transformer.Concrete.Environment
import           Control.Arrow.Transformer.Concrete.Random
import           Control.Arrow.Transformer.Concrete.Store

import qualified System.Random as R

import           GHC.Generics (Generic)

data Val = BoolVal Bool | NumVal Int deriving (Eq, Show, Generic)
type Addr = Label

run :: [LStatement] -> Failure String (HashMap Addr Val)
run ss =
  fst <$>
    runFailureT
      (runStoreT
        (runEnvT
          (runRandomT
             (runConcreteT
               (Generic.run ::
                 ConcreteT
                   (RandomT
                     (EnvT Text Addr
                       (StoreT Addr Val
                         (FailureT String
                          (->))))) [Statement] ())))))
      (M.empty,(M.empty,(R.mkStdGen 0, generate <$> ss)))

newtype ConcreteT c x y = ConcreteT { runConcreteT :: c x y }
  deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowFail e, ArrowEnv var addr env, ArrowStore addr val)
deriving instance ArrowFix x y c => ArrowFix x y (ConcreteT c)
deriving instance ArrowRand v c => ArrowRand v (ConcreteT c)

instance (ArrowChoice c, Profunctor c) => ArrowAlloc (Text,Val,Label) Addr (ConcreteT c) where
  alloc = arr $ \(_,_,l) -> l

instance (ArrowChoice c, ArrowFail String c) => IsVal Val (ConcreteT c) where
  boolLit = arr (\(b,_) -> BoolVal b)
  and = proc (v1,v2,_) -> case (v1,v2) of
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 && b2)
    _ -> fail -< "Expected two booleans as arguments for 'and'"
  or = proc (v1,v2,_) -> case (v1,v2) of
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 || b2)
    _ -> fail -< "Expected two booleans as arguments for 'or'"
  not = proc (v,_) -> case v of
    BoolVal b -> returnA -< BoolVal (Prelude.not b)
    _ -> fail -< "Expected a boolean as argument for 'not'"
  numLit = arr (\(d,_) -> NumVal d)
  add = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 + n2)
    _ -> fail -< "Expected two numbers as arguments for 'add'"
  sub = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 - n2)
    _ -> fail -< "Expected two numbers as arguments for 'sub'"
  mul = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 * n2)
    _ -> fail -< "Expected two numbers as arguments for 'mul'"
  div = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 `Prelude.div` n2)
    _ -> fail -< "Expected two numbers as arguments for 'mul'"
  eq = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2)   -> returnA -< BoolVal (n1 P.== n2)
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 P.== b2)
    _ -> fail -< "Expected two values of the same type as arguments for 'eq'"
  lt = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2)   -> returnA -< BoolVal (n1 P.< n2)
    _ -> fail -< "Expected two numbers as arguments for 'lt'"

instance (ArrowChoice c, ArrowFail String c) => ArrowCond Val (ConcreteT c) where
  type Join (ConcreteT c) x y = ()
  if_ f1 f2 = proc (v,(x,y)) -> case v of
    BoolVal True -> f1 -< x
    BoolVal False -> f2 -< y
    _ -> fail -< "Expected boolean as argument for 'if'"

instance R.Random Val where
  randomR (NumVal x,NumVal y) = first NumVal . R.randomR (x,y)
  randomR _ = error "random not defined for other values than numerical"
  random = first NumVal . R.random

instance Hashable Val
