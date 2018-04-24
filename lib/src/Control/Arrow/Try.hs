{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Arrow.Try where

import Prelude hiding (id,(.))

import Control.Category
import Control.Arrow
import Control.Arrow.Utils

import Control.Monad.Try

class Arrow c => ArrowTry x y z c where
  tryA :: c x y -> c y z -> c x z -> c x z

(<+>) :: (ArrowTry x y y c) => c x y -> c x y -> c x y
f <+> g = tryA f id g

success :: ArrowTry a a a c => c a a
success = id

instance MonadTry m => ArrowTry x y z (Kleisli m) where
  tryA (Kleisli f) (Kleisli g) (Kleisli h) = Kleisli $ \x -> try (f x) g (h x)

tryFirst :: (ArrowChoice c, ArrowTry (x, [x]) y y c) => c x y -> c () y -> c [x] y
tryFirst f g = proc l -> case l of
  [] -> g -< ()
  a:as -> tryA (f . pi1) id (tryFirst f g . pi2) -< (a,as)
