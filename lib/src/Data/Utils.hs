{-# LANGUAGE GADTs #-}
module Data.Utils
  ( maybeHead
  , fromMaybe
  , module Data.Empty
  , module Data.Singleton
  )
  where

import Data.Empty
import Data.Singleton

fromMaybe :: (IsEmpty (f a), IsSingleton (f a), Elem (f a) ~ a) => Maybe a -> f a
fromMaybe (Just a) = singleton a
fromMaybe Nothing  = empty

maybeHead :: [a] -> Maybe a
maybeHead (a:_) = Just a
maybeHead []    = Nothing
