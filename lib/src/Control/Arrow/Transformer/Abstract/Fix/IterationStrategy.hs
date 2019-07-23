{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Fix.IterationStrategy(
  IterationStrategy, trace, filter,
  module Control.Arrow.Transformer.Abstract.Fix.StackWidening,
  module Control.Arrow.Transformer.Abstract.Fix.Parallel,
  module Control.Arrow.Transformer.Abstract.Fix.Chaotic
) where

import Prelude hiding (pred,filter)

import Control.Arrow
import Control.Arrow.Fix
import Control.Arrow.Transformer.Abstract.Fix.StackWidening
import Control.Arrow.Transformer.Abstract.Fix.Parallel
import Control.Arrow.Transformer.Abstract.Fix.Chaotic

import Data.Profunctor
import Data.Lens (Prism',getMaybe,set)
import qualified Debug.Trace as Debug
import Text.Printf

trace :: (Show a, Show b, Arrow c) => IterationStrategy c a b -> IterationStrategy c a b
trace strat f = proc x -> do
  y <- strat f -< x 
  returnA -< Debug.trace (printf "CALL\nx: %s\nf(x) = %s\n\n" (show x) (show y)) y

filter :: (Profunctor c, ArrowChoice c, ArrowApply c) => Prism' a a' -> IterationStrategy c a' b -> IterationStrategy c a b
filter pred strat f = proc a -> case getMaybe pred a of
  Just a' -> strat (lmap (\x -> set pred x a) f) -<< a'
  Nothing -> f -< a


