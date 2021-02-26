{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Serialize where

import           Control.Arrow
import           Control.Arrow.Trans

import           Control.Arrow.Transformer.Concrete.Except
import           Control.Arrow.Transformer.Kleisli
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.Stack
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Writer

class ArrowSerialize val dat valTy datDecTy datEncTy c | c -> datDecTy, c -> datEncTy where
  decode :: c (val, x) y -> c x y -> c (dat, datDecTy, valTy, x) y
  encode :: c (dat, x) y -> c x y -> c (val, valTy, datEncTy, x) y

deriving instance (ArrowSerialize val dat valTy datDecTy datEncTy c) => ArrowSerialize val dat valTy datDecTy datEncTy (ValueT val2 c)
deriving instance (ArrowSerialize val dat valTy datDecTy datEncTy c) => ArrowSerialize val dat valTy datDecTy datEncTy (ExceptT e c)
instance (ArrowSerialize val dat valTy datDecTy datEncTy c) => ArrowSerialize val dat valTy datDecTy datEncTy (KleisliT f c) where
    decode a1 a2 = lift $
        (decode (unlift a1) (unlift a2))

    encode a1 a2 = lift $
        (encode (unlift a1) (unlift a2))
deriving instance (Arrow c, ArrowSerialize val dat valTy datDecTy datEncTy c) => ArrowSerialize val dat valTy datDecTy datEncTy (StackT v c)
instance (Arrow c, ArrowSerialize val dat valTy datDecTy datEncTy c) => ArrowSerialize val dat valTy datDecTy datEncTy (StateT s c) where
    decode a1 a2 = lift $ proc (s, (dat, datdecTy, valTy, x)) ->
        decode (proc (val, (s,x)) -> (unlift a1) -< (s, (val,x)))
               (unlift a2)
               -< (dat, datdecTy, valTy, (s,x))

    encode a1 a2 = lift $ proc (s, (val, valTy, datEncTy, x)) ->
        encode (proc (dat, (s,x)) -> (unlift a1) -< (s, (dat,x)))
               (unlift a2)
               -< (val, valTy, datEncTy, (s,x))
instance (Arrow c, ArrowSerialize val dat valTy datDecTy datEncTy c) => ArrowSerialize val dat valTy datDecTy datEncTy (ReaderT r c) where
    -- c1 :: c (r, (val, x)) y
    -- c2 :: c (r, x) y
    decode a1 a2 = lift $ proc (r, (dat, datdecTy, valTy, x)) -> do
        decode (proc (val, (r,x)) -> (unlift a1) -< (r, (val,x)))
               (unlift a2)
               -< (dat, datdecTy, valTy, (r,x))

    encode a1 a2 = lift $ proc (r, (val, valTy, datEncTy, x)) ->
        encode (proc (dat, (r,x)) -> (unlift a1) -< (r, (dat,x)))
               (unlift a2)
               -< (val, valTy, datEncTy, (r,x))
instance (ArrowSerialize val dat valTy datDecTy datEncTy c) => ArrowSerialize val dat valTy datDecTy datEncTy (WriterT w c) where
    -- TODO
