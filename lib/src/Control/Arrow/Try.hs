{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Arrow.Try where

import Prelude hiding (id,(.))

import Control.Category
import Control.Arrow
import Control.Arrow.Utils

-- | Arrow-based interface for handling exceptions.
class Arrow c => ArrowTry x y z c where
  -- | 'tryA f g h' runs the computation 'f', if it fails 'g' is
  -- executed on the result of 'f' and 'h' otherwise on the initial input.
  tryA :: c x y -> c y z -> c x z -> c x z

-- | Picks the first successful computation.
(<+>) :: ArrowTry x y y c => c x y -> c x y -> c x y
f <+> g = tryA f id g

tryFirst :: (ArrowChoice c, ArrowTry (x, [x]) y y c) => c x y -> c () y -> c [x] y
tryFirst f g = proc l -> case l of
  [] -> g -< ()
  a:as -> tryA (f . pi1) id (tryFirst f g . pi2) -< (a,as)

-- | A computation that always succeeds
success :: ArrowTry a a a c => c a a
success = id
