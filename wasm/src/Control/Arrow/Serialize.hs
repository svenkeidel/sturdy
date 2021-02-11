{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Serialize where

import           Control.Arrow.Transformer.Concrete.Except
import           Control.Arrow.Transformer.Kleisli
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.Stack
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Value

class ArrowSerialize val dat valTy datDecTy datEncTy c | c -> datDecTy, c -> datEncTy where
  decode :: c (val, x) y -> c x y -> c (dat, datdecTy, valTy, x) y
  encode :: c (dat, x) y -> c x y -> c (val, valTy, datEncTy, x) y

deriving instance (ArrowSerialize val dat valTy datDecTy datEncTy c) => ArrowSerialize val dat valTy datDecTy datEncTy (ValueT val2 c)
deriving instance (ArrowSerialize val dat valTy datDecTy datEncTy c) => ArrowSerialize val dat valTy datDecTy datEncTy (ExceptT e c)
instance (ArrowSerialize val dat valTy datDecTy datEncTy c) => ArrowSerialize val dat valTy datDecTy datEncTy (KleisliT f c) where
    -- TODO
deriving instance (ArrowSerialize val dat valTy datDecTy datEncTy c) => ArrowSerialize val dat valTy datDecTy datEncTy (StackT v c)
instance (ArrowSerialize val dat valTy datDecTy datEncTy c) => ArrowSerialize val dat valTy datDecTy datEncTy (StateT s c) where
    -- TODO
instance (ArrowSerialize val dat valTy datDecTy datEncTy c) => ArrowSerialize val dat valTy datDecTy datEncTy (ReaderT r c) where
    -- TODO
