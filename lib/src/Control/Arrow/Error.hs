{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Error where

import Control.Arrow

import Control.Monad.TryWithError

class Arrow c => ArrowError e x y c where
  tryWithErrorA :: c x y -> c y y -> c e y -> c x y

instance MonadTryWithError m => ArrowError String x y (Kleisli m) where
  tryWithErrorA (Kleisli f) (Kleisli g) (Kleisli h) = Kleisli $ \x -> tryWithError (f x) g h
