{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Class.Reader where

import Control.Arrow
import Control.Monad.Reader

class Arrow c => ArrowReader r c | c -> r where
  askA :: c () r
  localA :: c x y -> c (r,x) y

instance MonadReader r m => ArrowReader r (Kleisli m) where
  askA = Kleisli (const ask)
  localA (Kleisli f) = Kleisli (\(r,x) -> local (const r) (f x))
