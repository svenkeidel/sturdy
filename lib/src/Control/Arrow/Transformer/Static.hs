{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Static where

import Prelude hiding (id,(.),lookup,read,fail)

import Control.Category

import Control.Arrow
import Control.Arrow.Deduplicate
import Control.Arrow.Environment as Env
import Control.Arrow.Fail
import Control.Arrow.Except as Exc
import Control.Arrow.Trans
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store as Store
import Control.Arrow.Writer
import Control.Arrow.Abstract.Join
import Control.Arrow.Abstract.Terminating

import Data.Profunctor
import Data.Order hiding (lub)

-- Due to https://hackage.haskell.org/package/arrows/docs/Control-Arrow-Transformer-StaticT.html
newtype StaticT f c x y = StaticT { runStaticT :: f (c x y) }
  deriving (PreOrd,Complete,CoComplete,UpperBounded,LowerBounded)

instance (Applicative f, Profunctor c) => Profunctor (StaticT f c) where
  dimap f g (StaticT h) = StaticT $ dimap f g <$> h
  {-# INLINE dimap #-}
  lmap f (StaticT h) = StaticT $ lmap f <$> h
  {-# INLINE lmap #-}
  rmap g (StaticT h) = StaticT $ rmap g <$> h
  {-# INLINE rmap #-}

instance Applicative f => ArrowLift (StaticT f) where
  lift' = StaticT . pure
  {-# INLINE lift' #-}

instance (Applicative f, Arrow c, Profunctor c) => Category (StaticT f c) where
  id = lift' id
  {-# INLINE id #-}
  StaticT f . StaticT g = StaticT $ (.) <$> f <*> g
  {-# INLINE (.) #-}

instance (Applicative f, Arrow c, Profunctor c) => Arrow (StaticT f c) where
  arr = lift' . arr
  {-# INLINE arr #-}
  first (StaticT f) = StaticT $ first <$> f
  {-# INLINE first #-}
  second (StaticT f) = StaticT $ second <$> f
  {-# INLINE second #-}
  StaticT f *** StaticT g = StaticT $ (***) <$> f <*> g
  {-# INLINE (***) #-}
  StaticT f &&& StaticT g = StaticT $ (&&&) <$> f <*> g
  {-# INLINE (&&&) #-}

instance (Applicative f, ArrowChoice c, Profunctor c) => ArrowChoice (StaticT f c) where
  left (StaticT f) = StaticT $ left <$> f
  {-# INLINE left #-}
  right (StaticT f) = StaticT $ right <$> f
  {-# INLINE right #-}
  StaticT f +++ StaticT g = StaticT $ (+++) <$> f <*> g
  {-# INLINE (+++) #-}
  StaticT f ||| StaticT g = StaticT $ (|||) <$> f <*> g
  {-# INLINE (|||) #-}

instance (Applicative f, ArrowState s c) => ArrowState s (StaticT f c) where
  get = lift' get
  {-# INLINE get #-}
  put = lift' put
  {-# INLINE put #-}

instance (Applicative f, ArrowReader r c) => ArrowReader r (StaticT f c) where
  ask = lift' ask
  {-# INLINE ask #-}
  local (StaticT f) = StaticT $ local <$> f
  {-# INLINE local #-}

instance (Applicative f, ArrowWriter w c) => ArrowWriter w (StaticT f c) where
  tell = lift' tell
  {-# INLINE tell #-}

instance (Applicative f, ArrowFail e c) => ArrowFail e (StaticT f c) where
  fail = lift' fail
  {-# INLINE fail #-}

instance (Applicative f, ArrowTerminating c) => ArrowTerminating (StaticT f c) where
  throwTerminating = lift' throwTerminating
  {-# INLINE throwTerminating #-}
  catchTerminating (StaticT f) = StaticT $ catchTerminating <$> f
  {-# INLINE catchTerminating #-}

instance (Applicative f, ArrowExcept e c) => ArrowExcept e (StaticT f c) where
  type Join (StaticT f c) x y = Exc.Join c x y
  throw = lift' throw
  {-# INLINE throw #-}
  catch (StaticT f) (StaticT g) = StaticT $ catch <$> f <*> g
  {-# INLINE catch #-}
  finally (StaticT f) (StaticT g) = StaticT $ finally <$> f <*> g
  {-# INLINE finally #-}

instance (Applicative f, ArrowEnv var val env c) => ArrowEnv var val env (StaticT f c) where
  type Join (StaticT f c) x y = Env.Join c x y
  lookup (StaticT f) (StaticT g) = StaticT $ lookup <$> f <*> g
  {-# INLINE lookup #-}
  getEnv = lift' getEnv
  {-# INLINE getEnv #-}
  extendEnv = lift' extendEnv
  {-# INLINE extendEnv #-}
  localEnv (StaticT f) = StaticT $ localEnv <$> f
  {-# INLINE localEnv #-}

instance (Applicative f, ArrowStore var val c) => ArrowStore var val (StaticT f c) where
  type Join (StaticT f c) x y = Store.Join c x y
  read (StaticT f) (StaticT g) = StaticT $ read <$> f <*> g
  {-# INLINE read #-}
  write = lift' write
  {-# INLINE write #-}

instance (Applicative f, ArrowJoin c) => ArrowJoin (StaticT f c) where
  joinWith lub (StaticT f) (StaticT g) = StaticT $ joinWith lub <$> f <*> g
  {-# INLINE joinWith #-}

instance (Applicative f, ArrowDeduplicate x y c) => ArrowDeduplicate x y (StaticT f c) where
  dedup (StaticT f) = StaticT (dedup <$> f)
  {-# INLINE dedup #-}
