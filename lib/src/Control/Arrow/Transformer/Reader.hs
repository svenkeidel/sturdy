{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Control.Arrow.Transformer.Reader(ReaderT(..)) where

import Prelude hiding (id,(.),lookup,read,fail)

import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Conditional as Cond
import Control.Arrow.Environment as Env
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Reader
import Control.Arrow.Store as Store
import Control.Arrow.State
import Control.Arrow.Deduplicate
import Control.Arrow.Except as Exc
import Control.Arrow.Trans
import Control.Arrow.Writer
import Control.Arrow.Utils
import Control.Arrow.Abstract.Join
import Control.Arrow.Abstract.Terminating
import Control.Category

import Data.Profunctor
import Data.Order hiding (lub)
import Data.Monoidal

-- Due to "Generalising Monads to Arrows", by John Hughes, in Science of Computer Programming 37.
newtype ReaderT r c x y = ReaderT { runReaderT :: c (r,x) y }

instance (Profunctor c, Arrow c) => Profunctor (ReaderT r c) where
  dimap f g h = lift $ dimap (second f) g (unlift h)
  lmap f h = lift $ lmap (second f) (unlift h)
  rmap g h = lift $ rmap g (unlift h)

instance ArrowTrans (ReaderT r) where
  type Dom (ReaderT r) x y = (r,x)
  type Cod (ReaderT r) x y = y
  lift = ReaderT
  unlift = runReaderT

instance ArrowLift (ReaderT r) where
  lift' f = lift $ lmap snd f

instance (Arrow c, Profunctor c) => Category (ReaderT r c) where
  id    = lift' id
  f . g = lift $ lmap (\(r,x) -> (r,(r,x))) (unlift f . second (unlift g))

instance (Arrow c, Profunctor c) => Arrow (ReaderT r c) where
  arr f    = lift' (arr f)
  first f  = lift $ lmap (\(r,(b,d)) -> ((r,b),d)) $ first (unlift f)
  second f = lift $ lmap (\(r,(b,d)) -> (b,(r,d))) $ second (unlift f)
  f &&& g  = lift $ unlift f &&& unlift g
  f *** g  = lift $ lmap (\(r,(b,d)) -> ((r,b),(r,d))) $ unlift f *** unlift g

instance (ArrowChoice c, Profunctor c) => ArrowChoice (ReaderT r c) where
  left f  = lift $ lmap (\(r,e) -> left (r,) e) $ left (unlift f)
  right f = lift $ lmap (\(r,e) -> right (r,) e) $ right (unlift f)
  f +++ g = lift $ lmap distribute1 $ unlift f +++ unlift g
  f ||| g = lift $ lmap distribute1 $ unlift f ||| unlift g

instance (ArrowApply c, Profunctor c) => ArrowApply (ReaderT r c) where
  app = lift $ lmap (\(r,(f,b)) -> (unlift f,(r,b))) app

instance (Arrow c, Profunctor c) => ArrowReader r (ReaderT r c) where
  ask = lift pi1
  local f = lift $ lmap (\(_,(r,x)) -> (r,x)) (unlift f)

instance ArrowState s c => ArrowState s (ReaderT r c) where
  get = lift' get
  put = lift' put

instance ArrowWriter w c => ArrowWriter w (ReaderT r c) where
  tell = lift' tell

instance ArrowFail e c => ArrowFail e (ReaderT r c) where
  fail = lift' fail

instance ArrowTerminating c => ArrowTerminating (ReaderT r c) where
  throwTerminating = lift' throwTerminating
  catchTerminating f = lift $ catchTerminating (unlift f)

instance ArrowEnv var val env c => ArrowEnv var val env (ReaderT r c) where
  type instance Join (ReaderT r c) ((val,x),x) y = Env.Join c ((val,Dom (ReaderT r) x y),Dom (ReaderT r) x y) (Cod (ReaderT r) x y)
  lookup f g = lift $ lmap (\(r,(v,a)) -> (v,(r,a)))
                    $ lookup (lmap (\(v,(r,a)) -> (r,(v,a))) (unlift f)) (unlift g)
  getEnv     = lift' getEnv
  extendEnv  = lift' extendEnv
  localEnv f = lift $ lmap (\(r,(env,a)) -> (env,(r,a))) $ localEnv (unlift f)

instance ArrowStore var val c => ArrowStore var val (ReaderT r c) where
  type instance Join (ReaderT r c) ((val,x),x) y = Store.Join c ((val,Dom (ReaderT r) x y),Dom (ReaderT r) x y) (Cod (ReaderT r) x y)
  read f g = lift $ lmap (\(r,(v,a)) -> (v,(r,a)))
                  $ read (lmap (\(v,(r,a)) -> (r,(v,a))) (unlift f)) (unlift g)
  write = lift' write

type instance Fix x y (ReaderT r c) = ReaderT r (Fix (Dom (ReaderT r) x y) (Cod (ReaderT r) x y) c)
instance ArrowFix (Dom (ReaderT r) x y) (Cod (ReaderT r) x y) c => ArrowFix x y (ReaderT r c) where
  fix = liftFix

instance ArrowExcept e c => ArrowExcept e (ReaderT r c) where
  type instance Join (ReaderT r c) (x,(x,e)) y = Exc.Join c (Dom (ReaderT r) x y,(Dom (ReaderT r) x y,e)) (Cod (ReaderT r) x y)
  throw = lift' throw
  catch f g = lift $ catch (unlift f) (lmap assoc2 (unlift g))
  finally f g = lift $ finally (unlift f) (unlift g)

instance ArrowDeduplicate (r, x) y c => ArrowDeduplicate x y (ReaderT r c) where
  dedup f = lift (dedup (unlift f))

instance ArrowJoin c => ArrowJoin (ReaderT r c) where
  joinWith lub f g = lift $ joinWith lub (unlift f) (unlift g)

instance ArrowConst x c => ArrowConst x (ReaderT r c) where
  askConst = lift' askConst

instance ArrowCond v c => ArrowCond v (ReaderT r c) where
  type instance Join (ReaderT r c) (x,y) z = Cond.Join c (Dom (ReaderT r) x z,Dom (ReaderT r) y z) (Cod (ReaderT r) (x,y) z)
  if_ f g = lift $ lmap (\(r,(v,(x,y))) -> (v,((r,x),(r,y))))
                 $ if_ (unlift f) (unlift g)

deriving instance PreOrd (c (r,x) y) => PreOrd (ReaderT r c x y)
deriving instance LowerBounded (c (r,x) y) => LowerBounded (ReaderT r c x y)
deriving instance Complete (c (r,x) y) => Complete (ReaderT r c x y)
deriving instance CoComplete (c (r,x) y) => CoComplete (ReaderT r c x y)
deriving instance UpperBounded (c (r,x) y) => UpperBounded (ReaderT r c x y)
