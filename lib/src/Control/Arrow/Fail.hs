{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Fail where

import Control.Arrow

import Control.Monad.Except

class ArrowFail e c | c -> e where
  failA :: c e x

instance MonadError e m => ArrowFail e (Kleisli m) where
  failA = Kleisli $ throwError
