{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Arrow.Join where

import Control.Arrow hiding ((<+>))
import Control.Monad

class Arrow c => ArrowJoin c where
  (<+>) :: c x y -> c x y -> c x y

instance MonadPlus m => ArrowJoin (Kleisli m) where
  Kleisli f <+> Kleisli g = Kleisli $ \a -> f a `mplus` g a
