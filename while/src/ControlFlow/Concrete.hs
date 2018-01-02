{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}
module ControlFlow.Concrete where

import WhileLanguage
import ConcreteSemantics (Val(..), Store, initStore)

import ControlFlow.Prop

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Error
import Data.Order

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Arrow
import Control.Arrow.Fail

type M = StateT (Store,CProp) (Except String)
runM :: [Statement] -> Error String ((),(Store,CProp))
runM ss = fromEither $ runExcept $ runStateT (runKleisli run ss) (initStore, initCProp)

runConcrete :: [Statement] -> Error String ()
runConcrete ss = fmap fst $ runM ss

propConcrete :: [Statement] -> Error String LiftedCProp
propConcrete ss = fmap (liftCProp . reverse . snd . snd) $ runM ss

getStore :: M Store
getStore = get >>= return . fst

putStore :: Store -> M ()
putStore env = modify (\(x,y) -> (env,y))

modifyStore :: (Store -> Store) -> M ()
modifyStore f = modify (\(x,y) -> (f x, y))

putProp :: CProp -> M ()
putProp prop = modify (\(x,y) -> (x,prop))

modifyProp :: (CProp -> CProp) -> M ()
modifyProp f = modify (\(x,y) -> (x, f y))

instance ArrowFail String (Kleisli M) where
  failA = Kleisli $ \e -> throwError e
          
instance Run (Kleisli M) Val where
  fixRun f = voidA $ mapA $ f (fixRun f)

  store = Kleisli $ \(x,v,l) -> do
    modifyProp (TrAssign l v :)
    modifyStore (M.insert x v)

  if_ (Kleisli f1) (Kleisli f2) = Kleisli (\(v,(x,y),l) -> do
    modifyProp (TrIf l v :)
    case v of
      BoolVal True -> f1 x
      BoolVal False -> f2 y
      _ -> throwError "Expected boolean as argument for 'if'")

instance Eval (Kleisli M) Val where
  lookup = Kleisli $ \x -> do
    env <- getStore
    case M.lookup x env of
      Just v -> return v
      Nothing -> throwError "variable not found"

  boolLit = arr BoolVal

  and = proc (v1,v2) -> case (v1,v2) of
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 && b2)
    _ -> failA -< "Expected two booleans as arguments for 'and'"

  or = proc (v1,v2) -> case (v1,v2) of
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 || b2)
    _ -> failA -< "Expected two booleans as arguments for 'or'"

  not = proc v -> case v of
    BoolVal b -> returnA -< BoolVal (Prelude.not b)
    _ -> failA -< "Expected a boolean as argument for 'not'"

  numLit = arr NumVal

  add = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 + n2)
    _ -> failA -< "Expected two numbers as arguments for 'add'"

  sub = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 - n2)
    _ -> failA -< "Expected two numbers as arguments for 'sub'"

  mul = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 * n2)
    _ -> failA -< "Expected two numbers as arguments for 'mul'"

  div = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 / n2)
    _ -> failA -< "Expected two numbers as arguments for 'mul'"

  eq = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1,NumVal n2)   -> returnA -< BoolVal (n1 == n2)
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 == b2)
    _ -> failA -< "Expected two values of the same type as arguments for 'eq'"

  fixEval f = f (fixEval f)


