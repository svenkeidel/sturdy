{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Memory where

import           Control.Arrow
import           Control.Arrow.Trans

import           Control.Arrow.Transformer.Concrete.Except
import           Control.Arrow.Transformer.Kleisli
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.Stack
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Writer

import           Data.Profunctor

class ArrowMemory m addr bytes c | c -> addr, c -> bytes, c -> m where
  memread :: c (bytes, x) y -> c x y -> c (m, (addr, Int, x)) (m,y)
  memstore :: c x y -> c x y -> c (m, (addr, bytes, x)) (m,y)

--  getMemory :: c () m
--  putMemory :: c m ()
--
--withMemory :: (Arrow c, ArrowMemory m addr bytes c) => c x y -> c (m,x) (m,y)
--withMemory f = proc (m,x) -> do
--    putMemory -< m
--    y <- f -< x
--    newMem <- getMemory -< ()
--    returnA -< (m,y)

deriving instance (ArrowMemory m addr bytes c) => ArrowMemory m addr bytes (ValueT val2 c)
deriving instance (Arrow c, Profunctor c, ArrowMemory m addr bytes c) => ArrowMemory m addr bytes (ExceptT e c)
instance (Arrow c, Profunctor c, Functor f, ArrowMemory m addr bytes c) => ArrowMemory m addr bytes (KleisliT f c) where
    -- a1 :: KleisliT e c (bytes,x) y, a2 :: KleisliT e c x y
    -- c1 :: c (bytes,x) (e y), c2 :: c x (e y)
    -- memread :: c (bytes, x) y -> c x y -> c (m, (addr, Int, x)) (m,y)
    -- we need c (m, (addr, Int, x)) (m, y)
    memread a1 a2 = lift $
        -- lift :: c x (e y) -> KleisliT e c x y
        (memread (unlift a1) (unlift a2)) >>^ moveIn -- :: c (m, (addr, Int, x)) (e (m,y))
        -- moveIn :: (m, (e y)) -> (e (m,y))
        where moveIn (m, ey) = fmap ((,) m) ey

    memstore a1 a2 = lift $
        (memstore (unlift a1) (unlift a2)) >>^ moveIn
        where moveIn (m, ey) = fmap ((,) m) ey

deriving instance (Arrow c, ArrowMemory m addr bytes c) => ArrowMemory m addr bytes (StackT e c)
instance (Arrow c, ArrowMemory m addr bytes c) => ArrowMemory m addr bytes (StateT s c) where
    -- a1 :: StateT s c (bytes,x) y, a2 :: StateT s c x y
    -- c1 :: c (s, (bytes,x)) (s,y), c2 :: c (s,x) (s,y)
    -- memread :: c (bytes,x) y -> c x y -> c (m, (addr, Int, x)) (m,y)
    -- memread on StateT :: StateT s c (bytes,x) y -> StateT s c x y -> StateT s c (m,(addr,Int,x)) (m,y)
    -- StateT s c (m,(addr,bytes,x)) (m,y) has the form
    --          StateT $ c (s,(m,(addr,Int,x))) (s,(m,y))
    -- we need c (s, (m, (addr,Int,x))) (s, (m,y))
    memread a1 a2 = lift $ proc (s, (m, (addr,i,x))) -> do
    -- memread :: c (bytes,x) y -> c x y -> c (m, (addr, Int, x)) (m,y)
    -- memread :: c (bytes,(s,x)) (s,y) -> c (s,x) (s,y) -> c (m, (addr, Int, (s,x))) (m, (s,y))
        (newM, (newS,y)) <- memread (proc (bytes,(s,x)) -> unlift a1 -< (s, (bytes,x)))
                                    (unlift a2)
                                    -< (m, (addr,i,(s,x)))
        returnA -< (newS, (newM, y))
    
    memstore a1 a2 = lift $ proc (s, (m, (addr,bytes,x))) -> do
        (newM, (newS,y)) <- memstore (unlift a1) (unlift a2) -< (m, (addr,bytes,(s,x)))
        returnA -< (newS, (newM, y))

instance (Arrow c, ArrowMemory m addr bytes c) => ArrowMemory m addr bytes (ReaderT r c) where
    memread a1 a2 = lift $ proc (r, (m, (addr,i,x))) ->
        memread (proc (bytes, (r,x)) -> unlift a1 -< (r, (bytes,x)))
                (unlift a2)
                -< (m, (addr,i,(r,x)))
    memstore a1 a2 = lift $ proc (r, (m, (addr,bytes,x))) ->
        memstore (unlift a1) (unlift a2) -< (m, (addr, bytes, (r,x)))
instance (ArrowMemory m addr bytes c) => ArrowMemory m addr bytes (WriterT r c) where
    -- TODO
