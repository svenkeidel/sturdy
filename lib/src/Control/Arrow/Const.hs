{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Const where

import Control.Arrow

class Arrow c => ArrowConst r c | c -> r where
  askConst :: c () r
