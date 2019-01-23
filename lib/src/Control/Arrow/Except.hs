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

  -- | Type class constraint used by the abstract instances to join arrow computations.
  type family Join (c :: * -> * -> *) x y :: Constraint

  -- | Opertion that throws an exception that can be handled with 'catch'.
  throw :: c e a

  -- | @'catch' f g@ handles exceptions thrown in @f@ with @g@.
  catch :: Join c (x,(x,e)) y => c x y -> c (x,e) y -> c x y

  -- | @'finally' f g@ executes @g@, no matter if @f@ throws an exception.
  finally :: c x y -> c x u -> c x y

-- | Simpler version of 'throw'.
throw' :: ArrowExcept () c => c a b
throw' = proc _ -> throw -< ()

-- | Simpler version of 'catch'.
catch' :: (Join c (x,(x,e)) y, ArrowExcept e c) => c x y -> c e y -> c x y
catch' f g = catch f (pi2 >>> g)

-- | @'try' f g h@ executes @f@, if it succeeds the result is passed to
-- @g@, if it fails the original input is passed to @h@.
try :: (Join c (x,(x,e)) z, ArrowExcept e c) => c x y -> c y z -> c x z -> c x z
try f g h = catch (f >>> g) (pi1 >>> h)

-- | Picks the first computation that does not throw an exception.
(<+>) :: (Join c (x,(x,e)) y, ArrowExcept e c) => c x y -> c x y -> c x y
f <+> g = catch f (pi1 >>> g)

-- | @'tryFirst' f g -< l@ executes @f@ on elements of @l@ until one of them does not throw an exception.
-- In case @f@ throws an exception for all elements of @l@, @g@ is executed.
tryFirst :: (Join c ((x,[x]),((x,[x]),e)) y, ArrowChoice c, ArrowExcept e c) => c x y -> c () y -> c [x] y
tryFirst f g = proc l -> case l of
  [] -> g -< ()
  a:as -> try (f . pi1) id (tryFirst f g . pi2) -< (a,as)

-- | A computation that always succeeds
success :: ArrowExcept e c => c a a
success = id
