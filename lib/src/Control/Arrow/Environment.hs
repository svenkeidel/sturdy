{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Environment where

import Control.Arrow
import Control.Arrow.Utils

class Arrow c => ArrowEnv x y env c | c -> x, c -> y, c -> env where
  lookup :: c x y
  getEnv :: c () env
  extendEnv :: c (x,y,env) env
  localEnv :: c a b -> c (env,a) b

extendEnv' :: ArrowEnv x y env c => c a b -> c (x,y,a) b
extendEnv' f = proc (x,y,a) -> do
  env <- getEnv -< ()
  env' <- extendEnv -< (x,y,env)
  localEnv f -< (env',a)

bindings :: (ArrowChoice c, ArrowEnv x y env c) => c ([(x,y)],env) env
bindings = foldA ((\(env,(x,y)) -> (x,y,env)) ^>> extendEnv)
