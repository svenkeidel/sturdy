{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Effect where

import Control.Arrow

class Arrow c => ArrowEffect e c | c -> e where
  record :: (x -> e -> e) -> c x ()
