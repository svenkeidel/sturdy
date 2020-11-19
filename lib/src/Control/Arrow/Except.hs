{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Except where

import Prelude hiding (id,(.))

import Control.Category
import Control.Arrow hiding (ArrowMonad)
import Control.Arrow.Monad
import Control.Arrow.Trans
import Control.Arrow.Transformer.Cokleisli
import Control.Arrow.Transformer.Const
import Control.Arrow.Transformer.Kleisli
import Control.Arrow.Transformer.Reader
import Control.Arrow.Transformer.State
import Control.Arrow.Transformer.Static
import Control.Arrow.Transformer.Writer

import Data.Monoidal
import Data.Profunctor
import GHC.Exts (Constraint)

-- | Arrow-based interface for exception handling.
class (Arrow c, Profunctor c) => ArrowExcept e c | c -> e where
  type family Join y c :: Constraint

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

------------- Instances --------------
instance (ArrowComonad f c, ArrowExcept e c) => ArrowExcept e (CokleisliT f c) where
  type Join y (CokleisliT f c) = Join y c
  throw = lift' throw
  try f g h = lift $ try (mapDuplicateA (unlift f)) (unlift g) (lmap strength1 (unlift h))
  {-# INLINE throw #-}
  {-# INLINE try #-}

instance ArrowExcept e c => ArrowExcept e (ConstT r c) where
  type Join y (ConstT r c) = Join y c
  throw = lift $ \_ -> throw
  try f g h = lift $ \r -> try (unlift f r) (unlift g r) (unlift h r)
  {-# INLINE throw #-}
  {-# INLINE try #-}

instance (ArrowMonad f c, ArrowExcept e c) => ArrowExcept e (KleisliT f c) where
  type Join y (KleisliT f c) = Join (f y) c
  throw = lift' throw
  try f g h = lift $ try (unlift f) (mapJoinA (unlift g)) (unlift h)
  {-# INLINE throw #-}
  {-# INLINE try #-}

instance ArrowExcept e c => ArrowExcept e (ReaderT r c) where
  type Join z (ReaderT r c) = Join z c
  throw = lift' throw
  try f g h = lift $ try (lmap (\(r,x) -> (r,(r,x))) (second (unlift f))) (unlift g) (lmap assoc2 (unlift h))
  {-# INLINE throw #-}
  {-# INLINE try #-}

instance (ArrowExcept e c) => ArrowExcept e (StateT s c) where
  type Join y (StateT s c) = Join (s,y) c
  throw = lift (lmap snd throw)
  try f g h = lift $ try (unlift f) (unlift g) (lmap assoc2 (unlift h))
  {-# INLINE throw #-}
  {-# INLINE try #-}

instance (Applicative f, ArrowExcept e c) => ArrowExcept e (StaticT f c) where
  type Join y (StaticT f c) = Join y c
  throw = lift' throw
  try (StaticT f) (StaticT g) (StaticT h) = StaticT $ try <$> f <*> g <*> h
  {-# INLINE throw #-}
  {-# INLINE try #-}
  {-# SPECIALIZE instance ArrowExcept e c => ArrowExcept e (StaticT ((->) r) c) #-}

instance (Monoid w, ArrowExcept e c) => ArrowExcept e (WriterT w c) where
  type Join y (WriterT w c) = Join (w,y) c
  throw = lift' throw
  try f g h = lift $ try (unlift f) (rmap (\(w1,(w2,z)) -> (w1 <> w2,z)) (second (unlift g))) (unlift h)
  {-# INLINE throw #-}
  {-# INLINE try #-}
