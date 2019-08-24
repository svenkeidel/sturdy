{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module Control.Arrow.Cache where

import Control.Arrow
import Data.Profunctor
import Data.Abstract.Widening (Stable)

data Cached b = Compute | Cached (Stable,b)
  deriving (Show,Eq)

class (Arrow c, Profunctor c) => ArrowCache a b c | c -> a, c -> b where
  memoize :: c (a,Cached b) y -> c a y
  write :: c (a,b,Stable) ()
  update :: c (a,b) (Stable,b)
  setStable :: c (Stable,a) ()
