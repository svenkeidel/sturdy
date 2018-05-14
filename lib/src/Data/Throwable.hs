{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.Throwable where

class Throwable t where
  defaultError :: String -> t
