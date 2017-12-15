{-# LANGUAGE Arrows #-}
module Data.Complete where

import Prelude hiding (map)

import Control.Arrow

-- | Free completion for a partial order that is not complete
data Complete a = Complete a | Top

instance Functor Complete where
  fmap = map

map :: ArrowChoice c => c x y -> c (Complete x) (Complete y)
map f = proc c -> case c of
  Complete a -> Complete ^<< f -< a 
  Top -> returnA -< Top

instance Applicative Complete where
  pure = Complete
  Complete f <*> Complete x = Complete (f x)
  _ <*> _ = Top


