{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module GenericInterpreter where

import           Prelude hiding (succ, pred, fail)
import           Syntax (Expr(..))

import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Arrow.Fail
import           Control.Arrow.Environment(ArrowEnv)
import qualified Control.Arrow.Environment as Env

import           Data.Text (Text)

import           GHC.Exts (IsString(..),Constraint)

-- | The Generic interpreter for PCF. It uses the 'IsNum' and
-- 'IsClosure' interfaces to operate on numeric and closure
-- values. Furthermore, it uses 'ArrowFix', 'ArrowEnv', and
-- 'ArrowFail' to compute the fixpoint, operate on the environment,
-- and fail with an error message.
eval :: (ArrowChoice c, ArrowFix Expr v c, ArrowEnv Text v env c, ArrowFail e c, IsString e,
         IsNum v c, IsClosure v env c, Env.Join c ((v,Text),Text) v, Join c (Expr,Expr) v)
     => c Expr v
eval = fix $ \ev -> proc e0 -> case e0 of
  Var x _ -> Env.lookup' -< x
  Lam x e l -> do
    env <- Env.getEnv -< ()
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
    if_ ev ev -< (v1, (e2, e3))
  Y e l -> do
    fun <- ev -< e
    env <- Env.getEnv -< ()
    arg <- closure -< (Y e l, env)
    applyClosure' ev -< (fun, arg)
  Apply e _ -> ev -< e
  where
    -- Helper function used to apply closure or a suspended fixpoint computation to its argument.
    applyClosure' ev = applyClosure $ proc ((e,env),arg) -> case e of
      Lam x body l -> do
        env' <- Env.extendEnv -< (x,arg,env)
        Env.localEnv ev -< (env', Apply body l)
      Y e' l -> do
        fun' <- Env.localEnv ev -< (env, Y e' l)
        applyClosure' ev -< (fun',arg)
      _ -> fail -< fromString $ "found unexpected epxression in closure: " ++ show e

-- | Interface for numeric operations
class Arrow c => IsNum v c | c -> v where
  type family Join (c :: * -> * -> *) x y :: Constraint

  -- | increments the given number value.
  succ :: c v v

  -- | decrements the given number value.
  pred :: c v v

  -- | creates the numeric value zero.
  zero :: c () v

  if_ :: Join c (x,y) z => c x z -> c y z -> c (v, (x, y)) z


-- | Interface for closures
class Arrow c => IsClosure v env c | c -> env, c -> v where
  -- | creates a closure from an expression and an environment.
  closure :: c (Expr, env) v

  -- | applies a closure to an argument. The given continuation
  -- describes how to evaluated the body of the closure.
  applyClosure :: c ((Expr,env),v) v -> c (v, v) v
