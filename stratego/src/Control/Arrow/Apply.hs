{-# LANGUAGE Arrows #-}
module Control.Arrow.Apply where

import Control.Arrow

curry :: ArrowApply c => c (x,y) z -> c x (c y z)
curry f = arr (\x -> proc y -> f -< (x,y))

uncurry :: ArrowApply c => c x (c y z) -> c (x,y) z
uncurry f = proc (x,y) -> do
  f' <- f -< x
  app -< (f',y)

($$) :: ArrowApply c => c (x,y) z -> x -> c y z
($$) f x = proc y -> f -< (x,y)
