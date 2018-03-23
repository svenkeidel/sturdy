{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.State where

import Prelude hiding (id,(.))
import Control.Category
import Control.Arrow
import Control.Monad.State

class Arrow c => ArrowState s c | c -> s where
  getA :: c () s
  putA :: c s ()

modifyA :: ArrowState s c => c (x,s) s -> c x ()
modifyA f = proc x -> do
  s <- getA -< ()
  putA <<< f -< (x,s)

instance MonadState s m => ArrowState s (Kleisli m) where
  getA = Kleisli (const get)
  putA = Kleisli put
