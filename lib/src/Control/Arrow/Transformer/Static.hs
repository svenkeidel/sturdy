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
import Control.Arrow.Lift
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store as Store
import Control.Arrow.Writer
import Control.Arrow.Abstract.Join

import Data.Order hiding (lub)

-- Due to https://hackage.haskell.org/package/arrows/docs/Control-Arrow-Transformer-StaticT.html
newtype StaticT f c x y = StaticT { runStaticT :: f (c x y)}

instance Applicative f => ArrowLift (StaticT f) where
  lift = StaticT . pure

instance (Applicative f, Arrow c) => Category (StaticT f c) where
  id = lift id
  StaticT f . StaticT g = StaticT $ (.) <$> f <*> g

instance (Applicative f, Arrow c) => Arrow (StaticT f c) where
  arr = lift . arr
  first (StaticT f) = StaticT $ first <$> f
  second (StaticT f) = StaticT $ second <$> f
  StaticT f *** StaticT g = StaticT $ (***) <$> f <*> g
  StaticT f &&& StaticT g = StaticT $ (&&&) <$> f <*> g

instance (Applicative f, ArrowChoice c) => ArrowChoice (StaticT f c) where
  left (StaticT f) = StaticT $ left <$> f
  right (StaticT f) = StaticT $ right <$> f
  StaticT f +++ StaticT g = StaticT $ (+++) <$> f <*> g
  StaticT f ||| StaticT g = StaticT $ (|||) <$> f <*> g

instance (Applicative f, ArrowState s c) => ArrowState s (StaticT f c) where
  get = lift get
  put = lift put

instance (Applicative f, ArrowReader r c) => ArrowReader r (StaticT f c) where
  ask = lift ask
  local (StaticT f) = StaticT $ local <$> f

instance (Applicative f, ArrowWriter w c) => ArrowWriter w (StaticT f c) where
  tell = lift tell

instance (Applicative f, ArrowFail e c) => ArrowFail e (StaticT f c) where
  fail = lift fail

instance (Applicative f, ArrowExcept e c) => ArrowExcept e (StaticT f c) where
  type Join (StaticT f c) x y = Exc.Join c x y
  throw = lift throw
  catch (StaticT f) (StaticT g) = StaticT $ catch <$> f <*> g
  finally (StaticT f) (StaticT g) = StaticT $ finally <$> f <*> g

instance (Applicative f, ArrowEnv var val env c) => ArrowEnv var val env (StaticT f c) where
  type Join (StaticT f c) x y = Env.Join c x y
  lookup (StaticT f) (StaticT g) = StaticT $ lookup <$> f <*> g
  getEnv = lift getEnv
  extendEnv = lift extendEnv
  localEnv (StaticT f) = StaticT $ localEnv <$> f

instance (Applicative f, ArrowStore var val c) => ArrowStore var val (StaticT f c) where
  type Join (StaticT f c) x y = Store.Join c x y
  read (StaticT f) (StaticT g) = StaticT $ read <$> f <*> g
  write = lift write

instance (Applicative f, ArrowLoop c) => ArrowLoop (StaticT f c) where
  loop (StaticT f) = StaticT (loop <$> f)

instance (Applicative f, ArrowJoin c) => ArrowJoin (StaticT f c) where
  joinWith lub (StaticT f) (StaticT g) = StaticT $ joinWith lub <$> f <*> g

instance (Applicative f, ArrowDeduplicate x y c) => ArrowDeduplicate x y (StaticT f c) where
  dedup (StaticT f) = StaticT (dedup <$> f)

deriving instance PreOrd (f (c x y)) => PreOrd (StaticT f c x y)
deriving instance Complete (f (c x y)) => Complete (StaticT f c x y)
deriving instance CoComplete (f (c x y)) => CoComplete (StaticT f c x y)
deriving instance UpperBounded (f (c x y)) => UpperBounded (StaticT f c x y)
deriving instance LowerBounded (f (c x y)) => LowerBounded (StaticT f c x y)
