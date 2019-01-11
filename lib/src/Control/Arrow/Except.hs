{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Control.Arrow.Except where

import Prelude hiding (id,(.))

import Control.Category
import Control.Arrow
import Control.Arrow.Utils

import GHC.Exts(Constraint)

-- | Arrow-based interface for exception handling.
class Arrow c => ArrowExcept e c | c -> e where
  type family Join (c :: * -> * -> *) x y :: Constraint

  throw :: c e a

  -- | Executes the first computation. If it fails, the exception is
  -- handled with the second computation.
  catch :: Join c (x,(x,e)) y => c x y -> c (x,e) y -> c x y

  -- | Executes the second computation, no matter if the first
  -- computation fails or not.
  finally :: c x y -> c x u -> c x y

catch' :: (Join c (x,(x,e)) y, ArrowExcept e c) => c x y -> c e y -> c x y
catch' f g = catch f (pi2 >>> g)

-- | 'try f g h' Executes 'f', if it succeeds the result is passed to
-- 'g', if it fails the original input is passed to 'h'.
try :: (Join c (x,(x,e)) z, ArrowExcept e c) => c x y -> c y z -> c x z -> c x z
try f g h = catch (f >>> g) (pi1 >>> h)

-- | Picks the first successful computation.
(<+>) :: (Join c (x,(x,e)) y, ArrowExcept e c) => c x y -> c x y -> c x y
f <+> g = catch f (pi1 >>> g)

tryFirst :: (Join c ((x,[x]),((x,[x]),e)) y, ArrowChoice c, ArrowExcept e c) => c x y -> c () y -> c [x] y
tryFirst f g = proc l -> case l of
  [] -> g -< ()
  a:as -> try (f . pi1) id (tryFirst f g . pi2) -< (a,as)

-- | A computation that always succeeds
success :: ArrowExcept e c => c a a
success = id
