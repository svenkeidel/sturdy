{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Arrow.MaybeEnvironment where

import Control.Arrow
import Control.Arrow.Utils

class Arrow c => ArrowMaybeEnv x y env c | c -> x, c -> y, c -> env where
  lookup :: c x (Maybe y)
  getEnv :: c () env
  extendEnv :: c (x,y,env) env
  localEnv :: c a b -> c (env,a) b
