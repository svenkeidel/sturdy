{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Abstract.Stack where

class IsStack stack a b where
  push :: a -> stack a b -> stack a b
  peek :: stack a b -> Maybe a
