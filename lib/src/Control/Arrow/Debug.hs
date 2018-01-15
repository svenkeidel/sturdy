module Control.Arrow.Debug where

class ArrowDebug c where
  debug :: (Show a, Show b) => String -> c a b -> c a b
