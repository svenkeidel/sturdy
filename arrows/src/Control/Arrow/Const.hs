{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Const where

import Control.Arrow
import Data.Profunctor

-- | Arrow type class that gives access to a constant value.
class (Arrow c, Profunctor c) => ArrowConst r c | c -> r where

  -- | Retrieve the constant value.
  askConst :: (r -> c x y) -> c x y
