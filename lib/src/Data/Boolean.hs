{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Boolean where

class Logic b where
  true :: b
  false :: b
  and :: b -> b -> b
  or :: b -> b -> b
  not :: b -> b
  eq :: Eq a => a -> a -> b
