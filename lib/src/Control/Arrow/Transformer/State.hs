{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Control.Arrow.Transformer.State(StateT(..),evalStateT,execStateT) where

import Prelude hiding (id,(.),lookup,read,fail)

import Control.Arrow
import Control.Arrow.Alloc
import Control.Arrow.Const
import Control.Arrow.Conditional as Cond
import Control.Arrow.Deduplicate
import Control.Arrow.Environment as Env
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Trans
import Control.Arrow.Random
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store as Store
import Control.Arrow.Except as Exc
import Control.Arrow.Writer
import Control.Arrow.Utils

import Control.Arrow.Abstract.Join

import Control.Category

import Data.Hashable
import Data.Order hiding (lub)
import Data.Monoidal
import Data.Profunctor

-- Due to "Generalising Monads to Arrows", by John Hughes, in Science of Computer Programming 37.
newtype StateT s c x y = StateT { runStateT :: c (s,x) (s,y) }

evalStateT :: Arrow c => StateT s c x y -> c (s,x) y
evalStateT f = runStateT f >>> pi2
{-# INLINE evalStateT #-}

execStateT :: Arrow c => StateT s c x y -> c (s,x) s
execStateT f = runStateT f >>> pi1
{-# INLINE execStateT #-}

instance (Profunctor c, Arrow c) => Profunctor (StateT s c) where
  dimap f g h = lift $ dimap (second f) (second g) (unlift h)
  {-# INLINE dimap #-}
  lmap f h = lift $ lmap (second f) (unlift h)
  {-# INLINE lmap #-}
  rmap g h = lift $ rmap (second g) (unlift h)
  {-# INLINE rmap #-}

instance ArrowTrans (StateT s) where
  type Dom (StateT s) x y = (s,x)
  type Cod (StateT s) x y = (s,y)
  lift = StateT
  {-# INLINE lift #-}
  unlift = runStateT
  {-# INLINE unlift #-}

instance ArrowLift (StateT s) where
  lift' f = lift (second f)
  {-# INLINE lift' #-}

instance (Arrow c, Profunctor c) => Category (StateT s c) where
  id = lift' id
  {-# INLINE id #-}
  f . g = lift (unlift f . unlift g)
  {-# INLINE (.) #-}

instance (Arrow c, Profunctor c) => Arrow (StateT s c) where
  arr f = lift' (arr f)
  {-# INLINE arr #-}
  first f = lift $ dimap (\(s,(b,c)) -> ((s,b),c)) strength1 (first (unlift f))
  {-# INLINE first #-}
  second f = lift $ dimap (\(s,(a,b)) -> (a,(s,b))) strength2 (second (unlift f))
  {-# INLINE second #-}
  f &&& g = lift $ dimap (\(s,x) -> ((s,x),x)) (\((s,y),x) -> ((s,x),y)) (first (unlift f)) 
               >>> rmap (\((s,z),y) -> (s,(y,z))) (first (unlift g))
  {-# INLINE (&&&) #-}
  f *** g = lift $ dimap (\(s,(x,y)) -> ((s,x),y)) (\((s,y),x) -> ((s,x),y)) (first (unlift f))
               >>> rmap (\((s,z),y) -> (s,(y,z))) (first (unlift g)) 
  {-# INLINE (***) #-}

instance (ArrowChoice c, Profunctor c) => ArrowChoice (StateT s c) where
  left f = lift $ dimap (to distribute) (from distribute) (left (unlift f)) 
  {-# INLINE left #-}
  right f = lift $ dimap (to distribute) (from distribute) (right (unlift f))
  {-# INLINE right #-}
  f +++ g = lift $ dimap (to distribute) (from distribute) (unlift f +++ unlift g)
  {-# INLINE (+++) #-}
  f ||| g = lift $ lmap (to distribute) (unlift f ||| unlift g)
  {-# INLINE (|||) #-}

instance (ArrowApply c, Profunctor c) => ArrowApply (StateT s c) where
  app = StateT $ lmap (\(s,(StateT f,b)) -> (f,(s,b))) app
  {-# INLINE app #-}

instance (Arrow c, Profunctor c) => ArrowState s (StateT s c) where
  get = StateT (arr (\(a,()) -> (a,a)))
  {-# INLINE get #-}
  put = StateT (arr (\(_,s) -> (s,())))
  {-# INLINE put #-}


instance (ArrowFail e c, Profunctor c) => ArrowFail e (StateT s c) where
  fail = lift' fail
  {-# INLINE fail #-}

instance ArrowReader r c => ArrowReader r (StateT s c) where
  ask = lift' ask
  {-# INLINE ask #-}
  local f = lift $ lmap (\(s,(r,x)) -> (r,(s,x))) (local (unlift f))
  {-# INLINE local #-}

instance ArrowWriter w c => ArrowWriter w (StateT s c) where
  tell = lift' tell
  {-# INLINE tell #-}

instance (ArrowEnv var val env c) => ArrowEnv var val env (StateT s c) where
  type instance Join (StateT s c) ((val,x),x) y = Env.Join c ((val,Dom (StateT s) x y),Dom (StateT s) x y) (Cod (StateT s) x y)
  lookup f g = lift $ lmap (\(s,(v,a)) -> (v,(s,a)))
                    $ lookup (lmap (\(v,(s,a)) -> (s,(v,a))) (unlift f))
                             (unlift g)
  {-# INLINE lookup #-}
  getEnv = lift' getEnv
  {-# INLINE getEnv #-}
  extendEnv = lift' extendEnv
  {-# INLINE extendEnv #-}
  localEnv f = lift $ lmap (\(r,(env,a)) -> (env,(r,a))) (localEnv (unlift f))
  {-# INLINE localEnv #-}

instance (ArrowStore var val c) => ArrowStore var val (StateT s c) where
  type instance Join (StateT s c) ((val,x),x) y = Store.Join c ((val,Dom (StateT s) x y),Dom (StateT s) x y) (Cod (StateT s) x y)
  read f g = lift $ lmap (\(s,(v,a)) -> (v,(s,a)))
                  $ read (lmap (\(v,(s,a)) -> (s,(v,a))) (unlift f))
                         (unlift g)
  {-# INLINE read #-}
  write = lift' write
  {-# INLINE write #-}

type instance Fix x y (StateT s c) = StateT s (Fix (Dom (StateT s) x y) (Cod (StateT s) x y) c)
instance ArrowFix (s,x) (s,y) c => ArrowFix x y (StateT s c) where
  fix = liftFix
  {-# INLINE fix #-}

instance (ArrowExcept e c) => ArrowExcept e (StateT s c) where
  type instance Join (StateT s c) (x,(x,e)) y = Exc.Join c (Dom (StateT s) x y,(Dom (StateT s) x y,e)) (Cod (StateT s) x y)
  throw = lift' throw
  {-# INLINE throw #-}
  catch f g = lift $ catch (unlift f) (lmap (from assoc) (unlift g))
  {-# INLINE catch #-}
  finally f g = lift $ finally (unlift f) (unlift g)
  {-# INLINE finally #-}

instance (Eq s, Hashable s, ArrowDeduplicate (Dom (StateT s) x y) (Cod (StateT s) x y) c) => ArrowDeduplicate x y (StateT s c) where
  dedup f = lift (dedup (unlift f))
  {-# INLINE dedup #-}

instance (ArrowJoin c, Complete s) => ArrowJoin (StateT s c) where
  joinWith lub f g =
    lift $ joinWith (\(s1,z1) (s2,z2) -> (s1⊔s2,lub z1 z2)) (unlift f) (unlift g)
  {-# INLINE joinWith #-}

instance ArrowConst x c => ArrowConst x (StateT s c) where
  askConst = lift' askConst
  {-# INLINE askConst #-}

instance ArrowAlloc x y c => ArrowAlloc x y (StateT s c) where
  alloc = lift' alloc
  {-# INLINE alloc #-}

instance (ArrowCond v c) => ArrowCond v (StateT s c) where
  type instance Join (StateT s c) (x,y) z = Cond.Join c ((s,x),(s,y)) (s,z)
  if_ f g = lift $ lmap (\(s,(v,(x,y))) -> (v,((s,x),(s,y)))) (if_ (unlift f) (unlift g))
  {-# INLINE if_ #-}

instance ArrowRand v c => ArrowRand v (StateT s c) where
  random = lift' random
  {-# INLINE random #-}

deriving instance PreOrd (c (s,x) (s,y)) => PreOrd (StateT s c x y)
deriving instance LowerBounded (c (s,x) (s,y)) => LowerBounded (StateT s c x y)
deriving instance Complete (c (s,x) (s,y)) => Complete (StateT s c x y)
deriving instance CoComplete (c (s,x) (s,y)) => CoComplete (StateT s c x y)
deriving instance UpperBounded (c (s,x) (s,y)) => UpperBounded (StateT s c x y)
