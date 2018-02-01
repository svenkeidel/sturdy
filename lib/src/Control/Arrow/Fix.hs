{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Fix where

import Control.Arrow

class Arrow c => ArrowFix x y c | y -> c, x -> c where
  fixA :: (c x y -> c x y) -> c x y

