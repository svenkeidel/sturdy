{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Class.Fail where

import Control.Arrow

import Control.Monad.Except

class Arrow c => ArrowFail e c | c -> e where
  failA :: c e x

instance MonadError e m => ArrowFail e (Kleisli m) where
  failA = Kleisli throwError
