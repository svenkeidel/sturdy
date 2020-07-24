{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Store where

import           Prelude hiding (lookup,id,read,fail)

import           Control.Arrow hiding (ArrowMonad)
import           Control.Arrow.Monad
import           Control.Arrow.Trans
import           Control.Arrow.Transformer.Cokleisli
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Kleisli
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Static
import           Control.Arrow.Transformer.Writer
import           Control.Arrow.Fail (ArrowFail(fail))
import qualified Control.Arrow.Fail as Fail

import           Data.String
import           Data.Profunctor
import           Data.Monoidal

import           GHC.Exts (Constraint)

import           Text.Printf

-- | Arrow-based interface to describe computations that read from a store.
-- The parameter `y` needs to be exposed, because abstract instances
-- may need to join on `y`.
class (Arrow c, Profunctor c) => ArrowStore var val c | c -> var, c -> val where
  type family Join y c :: Constraint

  -- | Reads a value from the store. Fails if the binding is not in the current store.
  read :: Join y c => c (val,x) y -> c x y -> c (var,x) y

  -- | Writes a value to the store.
  write :: c (var,val) ()

-- | Simpler version of 'read'
read' :: (Show var, Join val c, Fail.Join val c, IsString e, ArrowFail e c, ArrowStore var val c) => c var val
read' = proc var ->
  read (proc (val,_) -> returnA -< val)
       (proc var     -> fail    -< fromString $ printf "variable %s not bound" (show var))
    -< (var,var)
{-# INLINE read' #-}

------------- Instances --------------
instance (ArrowComonad f c, ArrowStore var val c) => ArrowStore var val (CokleisliT f c) where
  type Join y (CokleisliT f c) = Join y c
  read f g = lift $ lmap costrength2 (read (lmap strength2 (unlift f)) (unlift g))
  write = lift' write
  {-# INLINE read #-}
  {-# INLINE write #-}

instance (ArrowStore var val c) => ArrowStore var val (ConstT r c) where
  type Join y (ConstT r c) = Join y c
  read f g = lift $ \r -> read (unlift f r) (unlift g r)
  write = lift $ \_ -> write
  {-# INLINE read #-}
  {-# INLINE write #-}

instance (ArrowMonad f c, ArrowStore var val c) => ArrowStore var val (KleisliT f c) where
  type Join y (KleisliT f c) = Join (f y) c
  read f g = lift $ read (unlift f) (unlift g)
  write = lift' write
  {-# INLINE read #-}
  {-# INLINE write #-}

instance ArrowStore var val c => ArrowStore var val (ReaderT r c) where
  type Join y (ReaderT r c) = Join y c
  read f g = lift $ lmap shuffle1
                  $ read (lmap shuffle1 (unlift f)) (unlift g)
  write = lift' write
  {-# INLINE read #-}
  {-# INLINE write #-}

instance (ArrowStore var val c) => ArrowStore var val (StateT s c) where
  type Join y (StateT s c) = Join (s,y) c
  read f g = lift $ lmap (\(s,(v,a)) -> (v,(s,a)))
                  $ read (lmap (\(v,(s,a)) -> (s,(v,a))) (unlift f))
                         (unlift g)
  write = lift' write
  {-# INLINE read #-}
  {-# INLINE write #-}

instance (Applicative f, ArrowStore var val c) => ArrowStore var val (StaticT f c) where
  type Join y (StaticT f c) = Join y c
  read (StaticT f) (StaticT g) = StaticT $ read <$> f <*> g
  write = lift' write
  {-# INLINE read #-}
  {-# INLINE write #-}
  {-# SPECIALIZE instance ArrowStore var val c => ArrowStore var val (StaticT ((->) r) c) #-}

instance (Monoid w, ArrowStore var val c) => ArrowStore var val (WriterT w c) where
  type Join y (WriterT w c) = Join (w,y) c
  read f g = lift $ read (unlift f) (unlift g)
  write = lift' write
  {-# INLINE read #-}
  {-# INLINE write #-}
