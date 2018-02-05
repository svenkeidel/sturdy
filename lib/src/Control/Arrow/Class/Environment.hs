{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Class.Environment where

import           Control.Arrow

class Arrow c => ArrowEnv x y env c | c -> x, c -> y, c -> env where
  lookup :: c x (Maybe y)
  getEnv :: c () env
  extendEnv :: c (x,y,env) env
  localEnv :: c a b -> c (env,a) b

extendEnv' :: ArrowEnv x y env c => c a b -> c (x,y,a) b
extendEnv' f = proc (x,y,a) -> do
  env <- getEnv -< ()
  env' <- extendEnv -< (x,y,env)
  localEnv f -< (env',a)
