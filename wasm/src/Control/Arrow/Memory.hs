{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Memory where

import           Control.Arrow
import           Control.Arrow.Trans

import           Control.Arrow.Transformer.Abstract.Except as AE
import           Control.Arrow.Transformer.Concrete.Except as CE
import           Control.Arrow.Transformer.Kleisli
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Writer

import           Data.Profunctor

import           GHC.Exts (Constraint)

class ArrowMemory addr bytes c | c -> addr, c -> bytes where
    type family Join y c :: Constraint

    -- | memread f g (ma,addr,size,x)
    -- | Reads `size` bytes from memory `ma` at address `addr` to retrieve `bytes`.
    -- | Invokes `f (bytes,x)` if all goes well.
    -- | Invokes `g x` if memory access is out of bounds.
    memread :: Join y c => c (bytes, x) y -> c x y -> c (Int, addr, Int, x) y
    memstore :: Join y c => c x y -> c x y -> c (Int, addr, bytes, x) y


deriving instance (ArrowMemory addr bytes c) => ArrowMemory addr bytes (ValueT val2 c)
deriving instance (Arrow c, Profunctor c, ArrowMemory addr bytes c) => ArrowMemory addr bytes (CE.ExceptT e c)
deriving instance (Arrow c, Profunctor c, ArrowMemory addr bytes c) => ArrowMemory addr bytes (AE.ExceptT e c)
instance (Arrow c, Profunctor c, Functor f, ArrowMemory addr bytes c) => ArrowMemory addr bytes (KleisliT f c) where
    type Join y (KleisliT f c) = Join (f y) c
    -- a1 :: KleisliT e c (bytes,x) y, a2 :: KleisliT e c x y
    -- c1 :: c (bytes,x) (e y), c2 :: c x (e y)
    -- memread :: c (bytes, x) y -> c x y -> c (m, (addr, Int, x)) (m,y)
    -- we need c (m, (addr, Int, x)) (m, y)
    memread a1 a2 = lift $
        -- lift :: c x (e y) -> KleisliT e c x y
        (memread (unlift a1) (unlift a2))-- >>^ moveIn -- :: c (m, (addr, Int, x)) (e (m,y))
        -- moveIn :: (m, (e y)) -> (e (m,y))
        where moveIn (m, ey) = fmap ((,) m) ey

    memstore a1 a2 = lift $
        (memstore (unlift a1) (unlift a2))-- >>^ moveIn
        where moveIn (m, ey) = fmap ((,) m) ey

instance (Arrow c, ArrowMemory addr bytes c) => ArrowMemory addr bytes (StateT s c) where
    type Join y (StateT s c) = Join (s,y) c
    memread a1 a2 = lift $ proc (s, (ma,addr,i,x)) -> do
        memread (proc (bytes,(s,x)) -> unlift a1 -< (s, (bytes,x)))
                (unlift a2)
                -< (ma,addr,i,(s,x))

    memstore a1 a2 = lift $ proc (s, (ma,addr,bytes,x)) -> do
        memstore (unlift a1) (unlift a2) -< (ma,addr,bytes,(s,x))

instance (Arrow c, ArrowMemory addr bytes c) => ArrowMemory addr bytes (ReaderT r c) where
    type Join y (ReaderT r c) = Join y c
    memread a1 a2 = lift $ proc (r, (ma,addr,i,x)) ->
        memread (proc (bytes, (r,x)) -> unlift a1 -< (r, (bytes,x)))
                (unlift a2)
                -< (ma,addr,i,(r,x))
    memstore a1 a2 = lift $ proc (r, (ma,addr,bytes,x)) ->
        memstore (unlift a1) (unlift a2) -< (ma, addr, bytes, (r,x))
instance (ArrowMemory addr bytes c) => ArrowMemory addr bytes (WriterT r c) where
    -- TODO
