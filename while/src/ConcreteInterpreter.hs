{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Concrete interpreter of the While language.
module ConcreteInterpreter where

import           Prelude hiding (read,fail,(.),id)
import qualified Prelude as P

import           Syntax
import           GenericInterpreter
import qualified GenericInterpreter as Generic

import           Data.Concrete.Error (Error)
import           Data.Hashable
import           Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Text (Text)
import           Data.Label
import           Data.Profunctor
import qualified Data.Function as Function

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fail as Fail
import           Control.Arrow.Random
import           Control.Arrow.Trans as Trans
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Concrete.Failure
import           Control.Arrow.Transformer.Concrete.Environment
import           Control.Arrow.Transformer.Concrete.Random
import           Control.Arrow.Transformer.Concrete.Store
import           Control.Arrow.Transformer.Concrete.Except

import qualified System.Random as R

import           GHC.Generics (Generic)

-- | Values of the While language can be booleans or numbers.
data Val = BoolVal Bool | NumVal Int deriving (Eq, Show, Generic)
type Addr = Label
type Env = HashMap Text Addr
type Store = HashMap Addr Val
type Exception = (Text,Val)

-- | The concrete interpreter of the while language instantiates
-- 'Generic.run' with the concrete components for failure ('FailureT'), store ('StoreT'),
-- environments ('EnvT'), random numbers ('RandomT'), and values ('ConcreteT').
run :: [LStatement] -> Error String (Error Exception (HashMap Addr Val))
run ss =
  let ?fixpointAlgorithm = Function.fix in 
  fmap fst <$>
    Trans.run
      (Generic.run ::
        ValueT Val
          (RandomT
            (EnvT Env
              (StoreT Store
                (ExceptT Exception
                  (FailureT String
                    (->)))))) [Statement] ())
      (M.empty,(M.empty,(R.mkStdGen 0, generate <$> ss)))

instance (ArrowChoice c, Profunctor c) => ArrowAlloc Addr (ValueT Val c) where
  alloc = arr $ \(_,_,l) -> l

instance (ArrowChoice c, ArrowFail String c, Fail.Join Val c) => IsVal Val (ValueT Val c) where
  type JoinVal y (ValueT Val c) = (Fail.Join y c)

  boolLit = arr BoolVal
  and = proc (v1,v2) -> case (v1,v2) of
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 && b2)
    _ -> fail -< "Expected two booleans as arguments for 'and'"
  or = proc (v1,v2) -> case (v1,v2) of
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 || b2)
    _ -> fail -< "Expected two booleans as arguments for 'or'"
  not = proc v -> case v of
    BoolVal b -> returnA -< BoolVal (Prelude.not b)
    _ -> fail -< "Expected a boolean as argument for 'not'"
  numLit = arr NumVal
  add = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 + n2)
    _ -> fail -< "Expected two numbers as arguments for 'add'"
  sub = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 - n2)
    _ -> fail -< "Expected two numbers as arguments for 'sub'"
  mul = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 * n2)
    _ -> fail -< "Expected two numbers as arguments for 'mul'"
  div = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 `Prelude.div` n2)
    _ -> fail -< "Expected two numbers as arguments for 'mul'"
  eq = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1,NumVal n2)   -> returnA -< BoolVal (n1 P.== n2)
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 P.== b2)
    _ -> fail -< "Expected two values of the same type as arguments for 'eq'"
  lt = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1,NumVal n2)   -> returnA -< BoolVal (n1 P.< n2)
    _ -> fail -< "Expected two numbers as arguments for 'lt'"
  if_ f1 f2 = proc (v,(x,y)) -> case v of
    BoolVal True -> f1 -< x
    BoolVal False -> f2 -< y
    _ -> fail -< "Expected boolean as argument for 'if'"

instance (Profunctor c, ArrowRand Int c) => ArrowRand Val (ValueT Val c) where
  random = ValueT $ proc () -> do
    r <- random -< ()
    returnA -< NumVal r

instance R.Random Val where
  randomR (NumVal x,NumVal y) = first NumVal . R.randomR (x,y)
  randomR _ = error "random not defined for other values than numerical"
  random = first NumVal . R.random

instance ArrowChoice c => IsException Exception Val (ValueT Val c) where
  type JoinExc y (ValueT Val c) = ()
  namedException = id
  matchException f g = proc (name,(name',v),x) ->
    if (name == name')
       then f -< (v,x)
       else g -< x

instance Hashable Val

