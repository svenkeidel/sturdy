{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  {-# INLINE dimap #-}
  lmap f h = lift $ lmap (second f) (unlift h)
  {-# INLINE lmap #-}
  rmap g h = lift $ rmap g (unlift h)
  {-# INLINE rmap #-}

instance ArrowTrans (ReaderT r) where
  type Dom (ReaderT r) x y = (r,x)
  type Cod (ReaderT r) x y = y
  lift = ReaderT
  {-# INLINE lift #-}
  unlift = runReaderT
  {-# INLINE unlift #-}

instance ArrowLift (ReaderT r) where
  lift' f = ReaderT (pi2 >>> f)
  {-# INLINE lift' #-}

instance (Arrow c, Profunctor c) => Category (ReaderT r c) where
  id    = lift' id
  {-# INLINE id #-}
  f . g = lift $ lmap (\(r,x) -> (r,(r,x))) (unlift f . second (unlift g))
  {-# INLINE (.) #-}

instance (Arrow c, Profunctor c) => Arrow (ReaderT r c) where
  arr f    = lift' (arr f)
  {-# INLINE arr #-}
  first f  = lift $ lmap (\(r,(b,d)) -> ((r,b),d)) $ first (unlift f)
  {-# INLINE first #-}
  second f = lift $ lmap (\(r,(b,d)) -> (b,(r,d))) $ second (unlift f)
  {-# INLINE second #-}
  f &&& g  = lift $ unlift f &&& unlift g
  {-# INLINE (&&&) #-}
  f *** g  = lift $ lmap (\(r,(b,d)) -> ((r,b),(r,d))) $ unlift f *** unlift g
  {-# INLINE (***) #-}

instance (ArrowChoice c, Profunctor c) => ArrowChoice (ReaderT r c) where
  left f  = lift $ lmap (to distribute >>> mmap id pi2) $ left (unlift f)
  {-# INLINE left #-}
  right f = lift $ lmap (to distribute >>> mmap pi2 id) $ right (unlift f)
  {-# INLINE right #-}
  f +++ g = lift $ lmap (to distribute) $ unlift f +++ unlift g
  {-# INLINE (+++) #-}
  f ||| g = lift $ lmap (to distribute) $ unlift f ||| unlift g
  {-# INLINE (|||) #-}

instance (ArrowApply c, Profunctor c) => ArrowApply (ReaderT r c) where
  app = lift $ lmap (\(r,(f,b)) -> (unlift f,(r,b))) app
  {-# INLINE app #-}

instance (Arrow c, Profunctor c) => ArrowReader r (ReaderT r c) where
  ask = lift pi1
  {-# INLINE ask #-}
  local f = lift $ lmap (\(_,(r,x)) -> (r,x)) (unlift f)
  {-# INLINE local #-}

instance ArrowState s c => ArrowState s (ReaderT r c) where
  get = lift' get
  {-# INLINE get #-}
  put = lift' put
  {-# INLINE put #-}

instance ArrowWriter w c => ArrowWriter w (ReaderT r c) where
  tell = lift' tell
  {-# INLINE tell #-}

instance ArrowFail e c => ArrowFail e (ReaderT r c) where
  fail = lift' fail
  {-# INLINE fail #-}

instance ArrowTerminating c => ArrowTerminating (ReaderT r c) where
  throwTerminating = lift' throwTerminating
  {-# INLINE throwTerminating #-}
  catchTerminating f = lift $ catchTerminating (unlift f)
  {-# INLINE catchTerminating #-}

instance ArrowEnv var val env c => ArrowEnv var val env (ReaderT r c) where
  type instance Join (ReaderT r c) ((val,x),x) y = Env.Join c ((val,Dom (ReaderT r) x y),Dom (ReaderT r) x y) (Cod (ReaderT r) x y)
  lookup f g = lift $ lmap (\(r,(v,a)) -> (v,(r,a)))
                    $ lookup (lmap (\(v,(r,a)) -> (r,(v,a))) (unlift f)) (unlift g)
  {-# INLINE lookup #-}
  getEnv     = lift' getEnv
  {-# INLINE getEnv #-}
  extendEnv  = lift' extendEnv
  {-# INLINE extendEnv #-}
  localEnv f = lift $ lmap (\(r,(env,a)) -> (env,(r,a))) $ localEnv (unlift f)
  {-# INLINE localEnv #-}

instance ArrowStore var val c => ArrowStore var val (ReaderT r c) where
  type instance Join (ReaderT r c) ((val,x),x) y = Store.Join c ((val,Dom (ReaderT r) x y),Dom (ReaderT r) x y) (Cod (ReaderT r) x y)
  read f g = lift $ lmap (\(r,(v,a)) -> (v,(r,a)))
                  $ read (lmap (\(v,(r,a)) -> (r,(v,a))) (unlift f)) (unlift g)
  {-# INLINE read #-}
  write = lift' write
  {-# INLINE write #-}

type instance Fix x y (ReaderT r c) = ReaderT r (Fix (Dom (ReaderT r) x y) (Cod (ReaderT r) x y) c)
instance ArrowFix (Dom (ReaderT r) x y) (Cod (ReaderT r) x y) c => ArrowFix x y (ReaderT r c) where
  fix = liftFix
  {-# INLINE fix #-}

instance ArrowExcept e c => ArrowExcept e (ReaderT r c) where
  type instance Join (ReaderT r c) (x,(x,e)) y = Exc.Join c (Dom (ReaderT r) x y,(Dom (ReaderT r) x y,e)) (Cod (ReaderT r) x y)
  throw = lift' throw
  {-# INLINE throw #-}
  catch f g = lift $ catch (unlift f) (lmap (from assoc) (unlift g))
  {-# INLINE catch #-}
  finally f g = lift $ finally (unlift f) (unlift g)
  {-# INLINE finally #-}

instance ArrowDeduplicate (r, x) y c => ArrowDeduplicate x y (ReaderT r c) where
  dedup f = lift (dedup (unlift f))
  {-# INLINE dedup #-}

instance ArrowJoin c => ArrowJoin (ReaderT r c) where
  joinWith lub f g = lift $ joinWith lub (unlift f) (unlift g)
  {-# INLINE joinWith #-}

instance ArrowConst x c => ArrowConst x (ReaderT r c) where
  askConst = lift' askConst
  {-# INLINE askConst #-}

instance ArrowCond v c => ArrowCond v (ReaderT r c) where
  type instance Join (ReaderT r c) (x,y) z = Cond.Join c (Dom (ReaderT r) x z,Dom (ReaderT r) y z) (Cod (ReaderT r) (x,y) z)
  if_ f g = lift $ lmap (\(r,(v,(x,y))) -> (v,((r,x),(r,y))))
                 $ if_ (unlift f) (unlift g)
  {-# INLINE if_ #-}


deriving instance PreOrd (c (r,x) y) => PreOrd (ReaderT r c x y)
deriving instance LowerBounded (c (r,x) y) => LowerBounded (ReaderT r c x y)
deriving instance Complete (c (r,x) y) => Complete (ReaderT r c x y)
deriving instance CoComplete (c (r,x) y) => CoComplete (ReaderT r c x y)
deriving instance UpperBounded (c (r,x) y) => UpperBounded (ReaderT r c x y)
