{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
module SharedSemantics where

import Prelude hiding (succ, pred, lookup)
import Syntax (Expr(..))

import Control.Arrow
import Control.Arrow.Fix
import Control.Arrow.Fail
import Control.Arrow.Environment

import Data.Text (Text)

-- | Shared interpreter for PCF.
eval :: (ArrowChoice c, ArrowFix Expr v c, ArrowEnv Text v env c, ArrowFail String c, IsVal v c, IsClosure v env c)
     => c Expr v
eval = fixA $ \ev -> proc e0 -> case e0 of
  Var x _ -> lookup -< x
  Lam x e l -> do
    env <- getEnv -< ()
    closure -< (Lam x e l, env)
  App e1 e2 _ -> do
    fun <- ev -< e1
    arg <- ev -< e2
    applyClosure' ev -< (fun, arg)
  Zero _ -> zero -< ()
  Succ e _ -> do
    v <- ev -< e
    succ -< v
  Pred e _ -> do
    v <- ev -< e
    pred -< v
  IfZero e1 e2 e3 _ -> do
    v1 <- ev -< e1
    ifZero ev ev -< (v1, (e2, e3))
  Y e l -> do
    fun <- ev -< e
    env <- getEnv -< ()
    arg <- closure -< (Y e l, env)
    applyClosure' ev -< (fun, arg)
  where
    -- Helper function used to apply closure or a suspended fixpoint computation to its argument.
    applyClosure' ev = applyClosure $ proc ((e,env),arg) -> case e of
      Lam x body _ -> do
        env' <- extendEnv -< (x,arg,env)
        localEnv ev -< (env', body)
      Y e' l -> do
        fun' <- localEnv ev -< (env, Y e' l)
        applyClosure' ev -< (fun',arg)
      _ -> failA -< "found unexpected epxression in closure: " ++ show e

-- | Interface for numeric operations
class Arrow c => IsVal v c | c -> v where
  -- | increments the given number value.
  succ :: c v v

  -- | decrements the given number value.
  pred :: c v v

  -- | creates the numeric value zero.
  zero :: c () v

  -- | performs a case distinction on the given numeric value. In case
  -- the number is zero, the first contiunation is executed, in case
  -- the number is not zero the second continuation is executed.
  ifZero :: c x v -> c y v -> c (v, (x, y)) v

-- | Interface for closures
class Arrow c => IsClosure v env c | c -> env, c -> v where
  -- | creates a closure from an expression and an environment.
  closure :: c (Expr, env) v

  -- | applies a closure to an argument. The given continuation
  -- describes how to evaluated the body of the closure.
  applyClosure :: c ((Expr,env),v) v -> c (v, v) v
