{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Fix where

import Prelude hiding ((.))

class ArrowFix c y | c -> y where
  fixA :: ((z -> c x y) -> (z -> c x y)) -> (z -> c x y)
