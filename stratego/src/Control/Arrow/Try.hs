{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Arrow.Try where

import Prelude hiding (id,(.),fail)

import Control.Category
import Control.Arrow

import Control.Monad.Try

class Arrow c => ArrowTry c where
  failA :: c x y
  tryA :: c x y -> c y z -> c x z -> c x z

success :: ArrowTry c => c a a
success = id
{-# INLINE success #-}

instance MonadTry m => ArrowTry (Kleisli m) where
  failA = Kleisli $ const fail
  tryA (Kleisli f) (Kleisli g) (Kleisli h) = Kleisli $ \x -> try (f x) g (h x)
