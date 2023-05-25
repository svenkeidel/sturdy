{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Serialize where

import           Control.Arrow
import           Control.Arrow.Trans

import qualified Control.Arrow.Transformer.Abstract.Store as AbsStore
import           Control.Arrow.Transformer.Concrete.Except as CE
import           Control.Arrow.Transformer.Abstract.Except as AE
import           Control.Arrow.Transformer.Kleisli
import           Control.Arrow.Transformer.Reader
--import           Control.Arrow.Transformer.Stack
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Writer

import qualified Data.Order as O
import           Data.Profunctor

class ArrowSerialize val dat valTy datDecTy datEncTy c | c -> datDecTy, c -> datEncTy where
  --decode :: c (val, x) y -> c x y -> c (dat, datDecTy, valTy, x) y
  decode :: c (dat, datDecTy, valTy) val
  --encode :: c (dat, x) y -> c x y -> c (val, valTy, datEncTy, x) y
  encode :: c (val, valTy, datEncTy) dat

deriving instance (ArrowSerialize val dat valTy datDecTy datEncTy c, Arrow c, Profunctor c) => ArrowSerialize val dat valTy datDecTy datEncTy (ValueT val2 c)
deriving instance (ArrowSerialize val dat valTy datDecTy datEncTy c, Arrow c, Profunctor c) => ArrowSerialize val dat valTy datDecTy datEncTy (CE.ExceptT e c)
deriving instance (O.Complete e, ArrowSerialize val dat valTy datDecTy datEncTy c, Arrow c, Profunctor c) => ArrowSerialize val dat valTy datDecTy datEncTy (AE.ExceptT e c)
instance (ArrowSerialize val dat valTy datDecTy datEncTy c, Arrow c, Profunctor c, Monad f) => ArrowSerialize val dat valTy datDecTy datEncTy (KleisliT f c) where
    decode = lift' decode
    encode = lift' encode
--deriving instance (Arrow c, ArrowSerialize val dat valTy datDecTy datEncTy c) => ArrowSerialize val dat valTy datDecTy datEncTy (StackT v c)
instance (Arrow c, ArrowSerialize val dat valTy datDecTy datEncTy c, Profunctor c) => ArrowSerialize val dat valTy datDecTy datEncTy (StateT s c) where
    decode = lift' decode
    encode = lift' encode
instance (Arrow c, ArrowSerialize val dat valTy datDecTy datEncTy c, Profunctor c) => ArrowSerialize val dat valTy datDecTy datEncTy (ReaderT r c) where
    decode = lift' decode
    encode = lift' encode
instance (ArrowSerialize val dat valTy datDecTy datEncTy c, Arrow c, Profunctor c, Monoid w) => ArrowSerialize val dat valTy datDecTy datEncTy (WriterT w c) where
    decode = lift' decode
    encode = lift' encode

deriving instance (Arrow c, ArrowSerialize val dat valTy datDecTy datEncTy c, Profunctor c) => ArrowSerialize val dat valTy datDecTy datEncTy (AbsStore.StoreT store c)
