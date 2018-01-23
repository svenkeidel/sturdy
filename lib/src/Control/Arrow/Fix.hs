{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Fix where

import Control.Arrow

class Arrow c => ArrowFix x y c | y -> c, x -> c where
  fixA :: (c x y -> c x y) -> c x y

class Arrow c => ArrowFix' c y | c -> y where
  fixA' :: ((z -> c x y) -> (z -> c x y)) -> (z -> c x y)
