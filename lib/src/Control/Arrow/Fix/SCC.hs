{-# LANGUAGE Arrows #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Fix.SCC where

import           Prelude hiding (head,iterate,map,elem)

import           Control.Arrow hiding (loop)
import           Control.Arrow.Fix.Stack (StackPointer)
import           Control.Arrow.Trans
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Static

import           Data.Profunctor


class (Arrow c, Profunctor c) => ArrowSCC a c | c -> a where
  -- | Adds an element to the strongly-connected component. The stack pointer
  -- indicates where the element occurs on the stack. This allows to implement
  -- SCCs more efficiently with bit sets.
  add :: c (a,StackPointer) ()

  -- | Removes an element from the strongly-connected component. The stack pointer
  -- indicates where the element occurs on the stack. This allows to implement
  -- SCCs more efficiently with bit sets.
  remove :: c (a,StackPointer) ()

  -- | Checks if a the current call is in the SCC.
  elem :: c a ElementSCC

  -- | Returns the size of the current SCC.
  size :: c () Int

  default add :: (c ~ t c', ArrowTrans t, ArrowSCC a c') => c (a,StackPointer) ()
  default remove :: (c ~ t c', ArrowTrans t, ArrowSCC a c') => c (a,StackPointer) ()
  default elem :: (c ~ t c', ArrowTrans t, ArrowSCC a c') => c a ElementSCC
  default size :: (c ~ t c', ArrowTrans t, ArrowSCC a c') => c () Int

  add = lift' add
  remove = lift' remove
  elem = lift' elem
  size = lift' size

  {-# INLINE add #-}
  {-# INLINE remove #-}
  {-# INLINE elem #-}
  {-# INLINE size #-}

data ElementSCC = InSCC StackPointer | NotInSCC

------------- Instances --------------
instance ArrowSCC a c => ArrowSCC a (ConstT r c)
instance ArrowSCC a c => ArrowSCC a (ReaderT r c)
instance ArrowSCC a c => ArrowSCC a (StateT s c)
instance (Applicative f, ArrowSCC a c) => ArrowSCC a (StaticT f c) where
