{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Try where

import Prelude hiding (id,(.),fail)

import Control.Category
import Control.Arrow
import Control.Arrow.Utils

import Control.Monad.Try

class Arrow c => ArrowTry c where
  tryA :: c x y -> c y z -> c x z -> c x z

success :: ArrowTry c => c a a
success = id

instance MonadTry m => ArrowTry (Kleisli m) where
  tryA (Kleisli f) (Kleisli g) (Kleisli h) = Kleisli $ \x -> try (f x) g (h x)

tryFirst :: (ArrowChoice c, ArrowTry c) => c x y -> c () y -> c [x] y
tryFirst f g = proc l -> case l of
  [] -> g -< ()
  a:as -> tryA (f . pi1) id (tryFirst f g . pi2) -< (a,as)
