{-# LANGUAGE Arrows #-}
module Control.Arrow.Transformer.CoState where

import Control.Arrow
    
-- The state comonad threads the state backwards through the computation.
newtype CoState s c x y = CoState (c (c s x,s) y)

runCoState :: Arrow c => CoState s c x y -> c (s,x) (s,y)
runCoState (CoState f) = proc (s,x) -> _ -< _
