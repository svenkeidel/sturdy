{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Fail where

class ArrowFail e c | c -> e where
  failA :: c e x
