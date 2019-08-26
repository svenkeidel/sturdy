{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Arrow.Cache where

import Control.Arrow
import Data.Profunctor
import Data.Abstract.Widening (Stable)

data Cached b = Compute | Cached (Stable,b)
  deriving (Show,Eq)

type ArrowCacheReuse a b c = (ArrowCache a b c, ArrowReuse a b c)

class (Arrow c, Profunctor c) => ArrowReuse a b c | c -> a, c -> b where
  reuse :: c (a,Cached b) y -> c a y

class (Arrow c, Profunctor c) => ArrowCache a b c | c -> a, c -> b where
  lookup :: c a (Maybe (Stable,b))
  write :: c (a,b,Stable) ()
  update :: c (a,b) (Stable,b)
  setStable :: c (Stable,a) ()
