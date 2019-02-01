{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
module Control.Arrow.Transformer.Abstract.Except(ExceptT(..)) where

import Prelude hiding (id,lookup,(.),read,fail)

import Control.Applicative
import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Deduplicate
import Control.Arrow.Environment as Env
import Control.Arrow.Fail
import Control.Arrow.Trans
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store as Store
import Control.Arrow.Except
import Control.Arrow.Fix
import Control.Arrow.Abstract.Join
import Control.Category

import Data.Abstract.Error
import Data.Monoidal
import Data.Order
import Data.Profunctor

newtype ExceptT e c x y = ExceptT { runExceptT :: c x (Error e y)}

instance (ArrowChoice c, Complete e, ArrowJoin c) => ArrowExcept e (ExceptT e c) where
  type Join (ExceptT e c) (x,(x,e)) y = Complete (c (y,(x,e)) (Error e y))
  throw = lift $ arr Fail
  {-# INLINE throw #-}
  catch f g = lift $ proc x -> do
    e <- unlift f -< x
    case e of
      Success y          -> returnA -< Success y
      SuccessOrFail er y -> joined (arr Success) (unlift g) -< (y,(x,er))
      Fail er            -> unlift g -< (x,er)
  {-# INLINE catch #-}
  finally f g = lift $ proc x -> do
    e <- unlift f -< x
    unlift g -< x
    returnA -< e
  {-# INLINE finally #-}

instance (ArrowChoice c, ArrowJoin c, Complete e) => Category (ExceptT e c) where
  id = lift' id
  {-# INLINE id #-}
  f . g = lift $ proc x -> do
    y <- unlift g -< x
    case y of
      Success y' -> unlift f -< y'
      Fail e     -> returnA -< Fail e
      SuccessOrFail e y' -> do
        -- Ideally we would like to write '(returnA -< Fail e) ⊔ (f -< y)',
        -- however this is not possible, because the result type of
        -- 'f', 'Error e z', is not 'Complete' because 'z' is not
        -- 'Complete'. However, in '(returnA -< Fail e) ⊔ (f -< y)' we
        -- actually never join to values of type 'z'.
        joinWith' (\(Fail e) er -> case er of
            Success z          -> SuccessOrFail e z
            Fail e'            -> Fail (e ⊔ e')
            SuccessOrFail e' z -> SuccessOrFail (e ⊔ e') z)
          id (unlift f) -< (Fail e,y')
  {-# INLINE (.) #-}

instance (Profunctor c, Arrow c) => Profunctor (ExceptT e c) where
  dimap f g h = lift $ dimap f (fmap g) (unlift h)
  {-# INLINE dimap #-}
  lmap f h = lift $ lmap f (unlift h)
  {-# INLINE lmap #-}
  rmap g h = lift $ rmap (fmap g) (unlift h)
  {-# INLINE rmap #-}

instance ArrowLift (ExceptT e) where
  lift' f = ExceptT (rmap Success f)
  {-# INLINE lift' #-}

instance ArrowTrans (ExceptT e) where
  type Dom (ExceptT e) x y = x
  type Cod (ExceptT e) x y = Error e y
  lift = ExceptT
  {-# INLINE lift #-}
  unlift = runExceptT
  {-# INLINE unlift #-}

instance (ArrowChoice c, ArrowJoin c, Complete e) => Arrow (ExceptT e c) where
  arr f    = lift' (arr f)
  {-# INLINE arr #-}
  first f  = lift $ rmap strength1 (first (unlift f))
  {-# INLINE first #-}
  second f = lift $ rmap strength2 (second (unlift f))
  {-# INLINE second #-}
  f &&& g = lift $ rmap mstrength (unlift f &&& unlift g)
  {-# INLINE (&&&) #-}
  f *** g = lift $ rmap mstrength (unlift f *** unlift g)
  {-# INLINE (***) #-}

instance (Complete e, ArrowJoin c, ArrowChoice c) => ArrowChoice (ExceptT e c) where
  left f  = lift $ rmap strength1 (left (unlift f))
  {-# INLINE left #-}
  right f = lift $ rmap strength2 (right (unlift f))
  {-# INLINE right #-}
  f ||| g = lift $ unlift f ||| unlift g
  {-# INLINE (|||) #-}
  f +++ g = lift $ rmap mstrength $ unlift f +++ unlift g
  {-# INLINE (+++) #-}

instance (Complete e, ArrowJoin c, ArrowApply c, ArrowChoice c) => ArrowApply (ExceptT e c) where
  app = lift $ lmap (first unlift) app
  {-# INLINE app #-}

instance (Complete e, ArrowJoin c, ArrowChoice c, ArrowState s c) => ArrowState s (ExceptT e c) where
  get = lift' get
  {-# INLINE get #-}
  put = lift' put
  {-# INLINE put #-}

instance (Complete e, ArrowJoin c, ArrowChoice c, ArrowStore var val c) => ArrowStore var val (ExceptT e c) where
  type Join (ExceptT e c) x y = Store.Join c (Dom (ExceptT e) x y) (Cod (ExceptT e) x y)
  read f g = lift $ read (unlift f) (unlift g)
  {-# INLINE read #-}
  write = lift' write
  {-# INLINE write #-}

instance (Complete e, ArrowJoin c, ArrowChoice c, ArrowFail f c) => ArrowFail f (ExceptT e c) where
  fail = lift' fail
  {-# INLINE fail #-}

instance (Complete e, ArrowJoin c, ArrowChoice c, ArrowReader r c) => ArrowReader r (ExceptT e c) where
  ask = lift' ask
  {-# INLINE ask #-}
  local f = lift (local (unlift f))
  {-# INLINE local #-}

instance (Complete e, ArrowJoin c, ArrowChoice c, ArrowEnv x y env c) => ArrowEnv x y env (ExceptT e c) where
  type Join (ExceptT e c) x y = Env.Join c (Dom (ExceptT e) x y) (Cod (ExceptT e) x y)
  lookup f g = lift $ lookup (unlift f) (unlift g)
  {-# INLINE lookup #-}
  getEnv = lift' getEnv
  {-# INLINE getEnv #-}
  extendEnv = lift' extendEnv
  {-# INLINE extendEnv #-}
  localEnv f = lift (localEnv (unlift f))
  {-# INLINE localEnv #-}

type instance Fix x y (ExceptT e c) = ExceptT e (Fix (Dom (ExceptT e) x y) (Cod (ExceptT e) x y) c)
instance (Complete e, ArrowJoin c, ArrowChoice c, ArrowFix (Dom (ExceptT e) x y) (Cod (ExceptT e) x y) c) => ArrowFix x y (ExceptT e c) where
  fix = liftFix
  {-# INLINE fix #-}

instance (Complete e, ArrowJoin c, ArrowChoice c) => ArrowDeduplicate x y (ExceptT e c) where
  dedup = returnA
  {-# INLINE dedup #-}

instance (Complete e, ArrowJoin c, ArrowChoice c, ArrowConst r c) => ArrowConst r (ExceptT e c) where
  askConst = lift' askConst
  {-# INLINE askConst #-}

instance (Complete e, ArrowJoin c, ArrowChoice c) => ArrowJoin (ExceptT e c) where
  joinWith lub' f g = ExceptT $ joinWith (widening (⊔) lub') (unlift f) (unlift g)
  {-# INLINE joinWith #-}

deriving instance PreOrd (c x (Error e y)) => PreOrd (ExceptT e c x y)
deriving instance LowerBounded (c x (Error e y)) => LowerBounded (ExceptT e c x y)
deriving instance Complete (c x (Error e y)) => Complete (ExceptT e c x y)
deriving instance CoComplete (c x (Error e y)) => CoComplete (ExceptT e c x y)
deriving instance UpperBounded (c x (Error e y)) => UpperBounded (ExceptT e c x y)
