{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Control.Arrow.Except where

import Prelude hiding (id,(.))

import Control.Category
import Control.Arrow

import Data.Profunctor

import GHC.Exts (Constraint)

-- | Arrow-based interface for exception handling.
class (Arrow c, Profunctor c) => ArrowExcept e c | c -> e where
  type family Join y (c :: * -> * -> *) :: Constraint

  -- | Opertion that throws an exception that can be handled with 'catch'.
  throw :: c e a

  -- | @'try' f g h@ executes @f@, if it succeeds the result is passed to
  -- @g@, if it fails the original input is passed to @h@.
  try :: Join z c => c x y -> c y z -> c (x,e) z -> c x z

-- | Simpler version of 'throw'.
throw' :: ArrowExcept () c => c a b
throw' = proc _ -> throw -< ()
{-# INLINE throw' #-}

try' :: (Join z c, ArrowExcept e c) => c x y -> c y z -> c x z -> c x z
try' f g h = try f g (lmap fst h)
{-# INLINE try' #-}

-- | @'catch' f g@ handles exceptions thrown in @f@ with @g@.
catch :: (Join y c, ArrowExcept e c) => c x y -> c (x,e) y -> c x y
catch f g = try f id g
{-# INLINE catch #-}
{-# ANN catch "HLint: ignore Eta reduce" #-}

-- | Simpler version of 'catch'.
catch' :: (Join y c, ArrowExcept e c) => c x y -> c e y -> c x y
catch' f g = catch f (lmap snd g)
{-# INLINE catch' #-}

-- | @'finally' f g@ executes @g@, no matter if @f@ throws an exception.
finally :: (Join y c, ArrowExcept e c) => c x y -> c x u -> c x y
finally f g = try (id &&& f)
                  (proc (x,y) -> do g -< x; returnA -< y)
                  (proc (x,e) -> do g -< x; throw -< e)
{-# INLINE finally #-}

-- | Picks the first computation that does not throw an exception.
(<+>) :: (Join y c, ArrowExcept e c) => c x y -> c x y -> c x y
f <+> g = catch f (lmap fst g)
{-# INLINE (<+>) #-}

-- | @'tryFirst' f g -< l@ executes @f@ on elements of @l@ until one of them does not throw an exception.
-- In case @f@ throws an exception for all elements of @l@, @g@ is executed.
tryFirst :: (Join y c, ArrowChoice c, ArrowExcept e c) => c x y -> c () y -> c [x] y
tryFirst f g = proc l -> case l of
  [] -> g -< ()
  a:as -> catch (lmap fst f) (lmap (snd . fst) (tryFirst f g)) -< (a,as)

-- | A computation that always succeeds
success :: ArrowExcept e c => c a a
success = id
{-# INLINE success #-}
