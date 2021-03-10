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
import           Control.Arrow.Transformer.Stack
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Writer

import qualified Data.Order as O

class ArrowSerialize val dat valTy datDecTy datEncTy c | c -> datDecTy, c -> datEncTy where
  --decode :: c (val, x) y -> c x y -> c (dat, datDecTy, valTy, x) y
  decode :: c (val, x) y -> c (dat, datDecTy, valTy, x) y
  --encode :: c (dat, x) y -> c x y -> c (val, valTy, datEncTy, x) y
  encode :: c (dat, x) y -> c (val, valTy, datEncTy, x) y

deriving instance (ArrowSerialize val dat valTy datDecTy datEncTy c) => ArrowSerialize val dat valTy datDecTy datEncTy (ValueT val2 c)
deriving instance (ArrowSerialize val dat valTy datDecTy datEncTy c) => ArrowSerialize val dat valTy datDecTy datEncTy (CE.ExceptT e c)
deriving instance (O.Complete e, ArrowSerialize val dat valTy datDecTy datEncTy c) => ArrowSerialize val dat valTy datDecTy datEncTy (AE.ExceptT e c)
instance (ArrowSerialize val dat valTy datDecTy datEncTy c) => ArrowSerialize val dat valTy datDecTy datEncTy (KleisliT f c) where
    decode a = lift $ decode (unlift a)

    encode a= lift $ encode (unlift a)
deriving instance (Arrow c, ArrowSerialize val dat valTy datDecTy datEncTy c) => ArrowSerialize val dat valTy datDecTy datEncTy (StackT v c)
instance (Arrow c, ArrowSerialize val dat valTy datDecTy datEncTy c) => ArrowSerialize val dat valTy datDecTy datEncTy (StateT s c) where
    decode a = lift $ proc (s, (dat, datdecTy, valTy, x)) ->
        decode (proc (val, (s,x)) -> (unlift a) -< (s, (val,x)))
               -< (dat, datdecTy, valTy, (s,x))

    encode a = lift $ proc (s, (val, valTy, datEncTy, x)) ->
        encode (proc (dat, (s,x)) -> (unlift a) -< (s, (dat,x)))
               -< (val, valTy, datEncTy, (s,x))
instance (Arrow c, ArrowSerialize val dat valTy datDecTy datEncTy c) => ArrowSerialize val dat valTy datDecTy datEncTy (ReaderT r c) where
    -- c1 :: c (r, (val, x)) y
    -- c2 :: c (r, x) y
    decode a = lift $ proc (r, (dat, datdecTy, valTy, x)) -> do
        decode (proc (val, (r,x)) -> (unlift a) -< (r, (val,x)))
               -< (dat, datdecTy, valTy, (r,x))

    encode a = lift $ proc (r, (val, valTy, datEncTy, x)) ->
        encode (proc (dat, (r,x)) -> (unlift a) -< (r, (dat,x)))
               -< (val, valTy, datEncTy, (r,x))
instance (ArrowSerialize val dat valTy datDecTy datEncTy c) => ArrowSerialize val dat valTy datDecTy datEncTy (WriterT w c) where
    -- TODO

deriving instance (Arrow c, ArrowSerialize val dat valTy datDecTy datEncTy c) => ArrowSerialize val dat valTy datDecTy datEncTy (AbsStore.StoreT store c)
