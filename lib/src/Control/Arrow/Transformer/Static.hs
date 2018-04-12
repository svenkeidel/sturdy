{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Arrow.Transformer.Static where

import Prelude hiding (id,(.),lookup,read)

import Control.Category

import Control.Arrow
import Control.Arrow.Environment
import Control.Arrow.Fail
import Control.Arrow.Lift
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store

import Data.Order

-- Due to https://hackage.haskell.org/package/arrows/docs/Control-Arrow-Transformer-Static.html
newtype Static f c x y = Static { runStatic :: f (c x y)}

instance Applicative f => ArrowLift (Static f) where
  lift = Static . pure

instance (Applicative f, Arrow c) => Category (Static f c) where
  id = lift id
  Static f . Static g = Static $ (.) <$> f <*> g

instance (Applicative f, Arrow c) => Arrow (Static f c) where
  arr = lift . arr
  first (Static f) = Static $ first <$> f
  second (Static f) = Static $ second <$> f
  Static f *** Static g = Static $ (***) <$> f <*> g
  Static f &&& Static g = Static $ (&&&) <$> f <*> g

instance (Applicative f, ArrowChoice c) => ArrowChoice (Static f c) where
  left (Static f) = Static $ left <$> f
  right (Static f) = Static $ right <$> f
  Static f +++ Static g = Static $ (+++) <$> f <*> g
  Static f ||| Static g = Static $ (|||) <$> f <*> g

instance (Applicative f, ArrowState s c) => ArrowState s (Static f c) where
  getA = lift getA
  putA = lift putA

instance (Applicative f, ArrowReader r c) => ArrowReader r (Static f c) where
  askA = lift askA
  localA (Static f) = Static $ localA <$> f

instance (Applicative f, ArrowFail e c) => ArrowFail e (Static f c) where
  failA = lift failA

instance (Applicative f, ArrowEnv x y env c) => ArrowEnv x y env (Static f c) where
  lookup = lift lookup
  getEnv = lift getEnv
  extendEnv = lift extendEnv
  localEnv (Static f) = Static $ localEnv <$> f

instance (Applicative f, ArrowStore var val c) => ArrowStore var val (Static f c) where
  read = lift read
  write = lift write

instance (Applicative f, ArrowLoop c) => ArrowLoop (Static f c) where
  loop (Static f) = Static (loop <$> f)

deriving instance PreOrd (f (c x y)) => PreOrd (Static f c x y)
deriving instance Complete (f (c x y)) => Complete (Static f c x y)
deriving instance CoComplete (f (c x y)) => CoComplete (Static f c x y)
deriving instance UpperBounded (f (c x y)) => UpperBounded (Static f c x y)
deriving instance LowerBounded (f (c x y)) => LowerBounded (Static f c x y)
         
