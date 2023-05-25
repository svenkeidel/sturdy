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

import qualified Data.Order as O
import           Data.Profunctor

import           GHC.Exts (Constraint)

class ArrowMemory addr bytes sz c | c -> addr, c -> bytes, c -> sz where
    type family Join y c :: Constraint

    -- | memread f g (ma,addr,size,x)
    -- | Reads `size` bytes from memory `ma` at address `addr` to retrieve `bytes`.
    -- | Invokes `f (bytes,x)` if all goes well.
    -- | Invokes `g x` if memory access is out of bounds.
    memread :: Join y c => c (bytes, x) y -> c x y -> c (Int, addr, Int, x) y
    memstore :: Join y c => c x y -> c x y -> c (Int, addr, bytes, x) y
    memsize :: c Int sz
    memgrow :: Join y c => c (sz,x) y -> c x y -> c (Int,sz,x) y


deriving instance (ArrowMemory addr bytes sz c) => ArrowMemory addr bytes sz (ValueT val2 c)
deriving instance (Arrow c, Profunctor c, ArrowMemory addr bytes sz c) => ArrowMemory addr bytes sz (CE.ExceptT e c)
deriving instance (O.Complete e, Arrow c, Profunctor c, ArrowMemory addr bytes sz c) => ArrowMemory addr bytes sz (AE.ExceptT e c)
instance (Arrow c, Profunctor c, Monad f, ArrowMemory addr bytes sz c) => ArrowMemory addr bytes sz (KleisliT f c) where
    type Join y (KleisliT f c) = Join (f y) c
    memread a1 a2 = lift $
        memread (unlift a1) (unlift a2)
    memstore a1 a2 = lift $
        memstore (unlift a1) (unlift a2)
    memsize = lift' memsize
    memgrow a1 a2 = lift $
        memgrow (unlift a1) (unlift a2)

instance (Arrow c, Profunctor c, ArrowMemory addr bytes sz c) => ArrowMemory addr bytes sz (StateT s c) where
    type Join y (StateT s c) = Join (s,y) c
    memread a1 a2 = lift $ proc (s, (ma,addr,i,x)) -> do
        memread (proc (bytes,(s,x)) -> unlift a1 -< (s, (bytes,x)))
                (unlift a2)
                -< (ma,addr,i,(s,x))
    memstore a1 a2 = lift $ proc (s, (ma,addr,bytes,x)) -> do
        memstore (unlift a1) (unlift a2) -< (ma,addr,bytes,(s,x))
    memsize = lift' memsize
    memgrow a1 a2 = lift $ proc (s, (ma,sz,x)) -> do
        memgrow (proc (sz,(s,x)) -> unlift a1 -< (s, (sz,x)))
                (unlift a2)
                -< (ma,sz,(s,x))

instance (Arrow c, Profunctor c, ArrowMemory addr bytes sz c) => ArrowMemory addr bytes sz (ReaderT r c) where
    type Join y (ReaderT r c) = Join y c
    memread a1 a2 = lift $ proc (r, (ma,addr,i,x)) ->
        memread (proc (bytes, (r,x)) -> unlift a1 -< (r, (bytes,x)))
                (unlift a2)
                -< (ma,addr,i,(r,x))
    memstore a1 a2 = lift $ proc (r, (ma,addr,bytes,x)) ->
        memstore (unlift a1) (unlift a2) -< (ma, addr, bytes, (r,x))
    memsize = lift' memsize
    memgrow a1 a2 = lift $ proc (r, (ma,sz,x)) ->
        memgrow (proc (sz, (r,x)) -> unlift a1 -< (r, (sz,x)))
                (unlift a2)
                -< (ma,sz,(r,x))
instance (Arrow c, Profunctor c, Monoid r, ArrowMemory addr bytes sz c) => ArrowMemory addr bytes sz (WriterT r c) where
    type Join x (WriterT r c) = Join (r,x) c
    memread = error "TODO: implement WriterT.memread"
    memstore = error "TODO: implement WriterT.memstore"
    memsize = lift' memsize
    memgrow = error "TODO: implement WriterT.memgrow"
