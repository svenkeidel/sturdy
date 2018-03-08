{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.Label where

-- Retrieves label from expression.
class Label x l | x -> l where
  getLabel :: x -> l
