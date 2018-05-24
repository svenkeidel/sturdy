{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Const where

import Control.Arrow

-- | Arrow-based interface that gives access to a constant value.
class Arrow c => ArrowConst r c | c -> r where
  -- | Retrieve the constant value.
  askConst :: c () r
