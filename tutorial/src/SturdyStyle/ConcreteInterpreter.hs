{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module SturdyStyle.ConcreteInterpreter where

import           Prelude hiding (lookup,and,fail)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Arrow.Environment
import           Control.Arrow.Store
import           Control.Arrow.Fail
import           Control.Arrow.Alloc
import           Control.Arrow.Transformer.Concrete.Failure
import           Control.Arrow.Transformer.Concrete.Environment
import           Control.Arrow.Transformer.Concrete.Store

import           Data.Concrete.Error (Error)
import           Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Profunctor
import           Data.Label

import           SturdyStyle.GenericInterpreter(IsValue(..))
import qualified SturdyStyle.GenericInterpreter as Generic
import           Syntax

type Addr = Label
data Val = BoolVal Bool | NumVal Int

instance (ArrowChoice c, ArrowFail String c) => IsValue Val (ConcreteT c) where
  numLit = proc n -> returnA -< NumVal n
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

  if_ f g = proc (v,x,y) -> case v of
    BoolVal True -> f -< x
    BoolVal False -> g -< y
    _ -> fail -< "Expected a boolean expression as condition for an if"

instance (Profunctor c, Arrow c) => ArrowAlloc (String, Val, Label) Label (ConcreteT c) where
  alloc = proc (_,_,l) -> returnA -< l

run :: [LStatement] -> Error String (HashMap Addr Val)
run stmts = fst <$>
  runFailureT
    (runStoreT
      (runEnvT
        (runConcreteT
          (Generic.run ::
            ConcreteT
              (EnvT String Addr
                (StoreT Addr Val
                  (FailureT String
                   (->)))) [Statement] ()))))
      (M.empty,(M.empty,generate <$> stmts))

newtype ConcreteT c x y = ConcreteT { runConcreteT :: c x y }
  deriving (Category,Profunctor,Arrow,ArrowChoice,ArrowFail e,ArrowEnv var addr env,ArrowStore addr val)
deriving instance ArrowFix x y c => ArrowFix x y (ConcreteT c)
type instance Fix x y (ConcreteT c) = ConcreteT (Fix x y c)

