{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Arrow.Deduplicate where

import Control.Arrow
import Data.Profunctor

-- | Arrow-based interface to deduplicate the result /set/ of a computation.
-- This is required by the 'Control.Arrow.Transformer.Abstract.Powerset.PowT'
-- arrow transformer.
class (Arrow c, Profunctor c) => ArrowDeduplicate x y c where
  dedup :: c x y -> c x y

instance ArrowDeduplicate x y (->) where
  dedup = returnA
