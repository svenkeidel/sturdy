{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Class.Fix(ArrowFix(..)) where

import Control.Arrow

class Arrow c => ArrowFix x y c | c -> y, c -> x where
  fixA :: (c x y -> c x y) -> c x y
