{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Except where

import Prelude hiding (id,(.))

import Control.Category
import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Utils

-- | Arrow-based interface for exception handling.
class ArrowFail e c => ArrowExcept x y e c | c -> e where
  -- | Executes the first computation. If it fails, the exception is
  -- handled with the second computation.
  tryCatchA :: c x y -> c (x,e) y -> c x y

tryCatchA' :: ArrowExcept x y e c => c x y -> c e y -> c x y
tryCatchA' f g = tryCatchA f (pi2 >>> g)

-- | Executes the second computation, no matter if the first
-- computation fails or not.
finally :: (ArrowChoice c, ArrowExcept x (Either x z) e c) => c x y -> c x z -> c x z
finally f g = tryCatchA (proc x     -> do _ <- f -< x; returnA -< Left x)
                        (proc (x,_) -> do y <- g -< x; returnA -< Right y)
          >>> (g ||| id)

-- | 'tryA f g h' Executes 'f', if it succeeds the result is passed to
-- 'g', if it fails the original input is passed to 'h'.
tryA :: ArrowExcept x z e c => c x y -> c y z -> c x z -> c x z
tryA f g h = tryCatchA (f >>> g) (pi1 >>> h)

-- | Picks the first successful computation.
(<+>) :: ArrowExcept x y e c => c x y -> c x y -> c x y
f <+> g = tryCatchA f (pi1 >>> g)

tryFirst :: (ArrowChoice c, ArrowExcept (x, [x]) y e c) => c x y -> c () y -> c [x] y
tryFirst f g = proc l -> case l of
  [] -> g -< ()
  a:as -> tryA (f . pi1) id (tryFirst f g . pi2) -< (a,as)

-- | A computation that always succeeds
success :: ArrowExcept a a e c => c a a
success = id
