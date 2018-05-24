{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Fail where

import Control.Arrow

import Control.Monad.Except

-- | Arrow-based interface for computations that can fail.
class Arrow c => ArrowFail e c | c -> e where
  -- | Throws an exception of type `e`.
  failA :: c e x

instance MonadError e m => ArrowFail e (Kleisli m) where
  failA = Kleisli throwError

failA' :: ArrowFail () c => c a b
failA' = arr (const ()) >>> failA
