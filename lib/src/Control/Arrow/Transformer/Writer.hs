{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Arrow.Transformer.Writer where

import Prelude hiding (id,(.),lookup,read,fail)

import Control.Category
import Control.Arrow
import Control.Arrow.State
import Control.Arrow.Reader
import Control.Arrow.Fail
import Control.Arrow.Lift
import Control.Arrow.Fix
import Control.Arrow.Except
import Control.Arrow.Store
import Control.Arrow.Environment
import Control.Arrow.Writer
import Control.Arrow.Abstract.Join

import Data.Monoid
import Data.Monoidal
import Data.Order hiding (lub)

newtype Writer w c x y = Writer { runWriter :: c x (w,y) }

instance Monoid w => ArrowLift (Writer w) where
  lift f = Writer (arr (const mempty) &&& f)

instance (Monoid w, Arrow c) => Category (Writer w c) where
  id = Writer (arr mempty &&& id)
  Writer g . Writer f = Writer $ proc x -> do
    (w1,y) <- f -< x
    (w2,z) <- g -< y
    returnA -< (w1 <> w2,z)

instance (Monoid w, Arrow c) => Arrow (Writer w c) where
  arr f = Writer (arr mempty &&& arr f)
  first (Writer f) = Writer (first f >>^ (\((w,b),d) -> (w,(b,d))))
  second (Writer g) = Writer (second g >>^ (\(d,(w,b)) -> (w,(d,b))))
  Writer f *** Writer g = Writer (f *** g >>^ (\((w1,b),(w2,d)) -> (w1 <> w2,(b,d))))
  Writer f &&& Writer g = Writer (f &&& g >>^ (\((w1,b),(w2,d)) -> (w1 <> w2,(b,d))))

instance (Monoid w, ArrowChoice c) => ArrowChoice (Writer w c) where
  left (Writer f) = Writer (left f >>^ (\e -> case e of Left (w,x) -> (w,Left x); Right y -> (mempty,Right y)))
  right (Writer f) = Writer (right f >>^ (\e -> case e of Left x -> (mempty,Left x); Right (w,y) -> (w,Right y)))
  Writer f ||| Writer g = Writer (f ||| g)
  Writer f +++ Writer g = Writer (f +++ g >>^ from distribute)

instance (Monoid w, ArrowApply c) => ArrowApply (Writer w c) where
  app = Writer $ (\(Writer f,x) -> (f,x)) ^>> app

instance (Monoid w, ArrowState s c) => ArrowState s (Writer w c) where
  get = lift get
  put = lift put

instance (Monoid w, Arrow c) => ArrowWriter w (Writer w c) where
  tell = Writer (arr (\w -> (w,())))

instance (Monoid w, ArrowFail e c) => ArrowFail e (Writer w c) where
  fail = lift fail

instance (Monoid w, ArrowExcept x (w,y) e c) => ArrowExcept x y e (Writer w c) where
  tryCatch (Writer f) (Writer g) = Writer $ tryCatch f g
  finally (Writer f) (Writer g) = Writer $ finally f g

instance (Monoid w, ArrowReader r c) => ArrowReader r (Writer w c) where
  ask = lift ask
  local (Writer f) = Writer (local f)

instance (Monoid w, ArrowEnv x y env c) => ArrowEnv x y env (Writer w c) where
  lookup (Writer f) (Writer g) = Writer $ lookup f g
  getEnv = lift getEnv
  extendEnv = lift extendEnv
  localEnv (Writer f) = Writer (localEnv f)

instance (Monoid w, ArrowRead var val x (w,y) c)  => ArrowRead var val x y (Writer w c) where
  read (Writer f) (Writer g) = Writer $ read f g

instance (Monoid w, ArrowWrite var val c)  => ArrowWrite var val (Writer w c) where
  write = lift write

type instance Fix x y (Writer w c) = Writer w (Fix x (w,y) c)
instance (Monoid w, ArrowFix x (w,y) c) => ArrowFix x y (Writer w c) where
  fix f = Writer (fix (runWriter . f . Writer))

instance (Monoid w, ArrowLoop c) => ArrowLoop (Writer w c) where
  loop (Writer f) = Writer $ loop (f >>^ to assoc)

instance (Monoid w, Complete w, ArrowJoin c) => ArrowJoin (Writer w c) where
  joinWith lub (Writer f) (Writer g) = Writer $ joinWith (\(w1,z1) (w2,z2) -> (w1 ⊔ w2, lub z1 z2)) f g

deriving instance PreOrd (c x (w,y)) => PreOrd (Writer w c x y)
deriving instance LowerBounded (c x (w,y)) => LowerBounded (Writer w c x y)
deriving instance Complete (c x (w,y)) => Complete (Writer w c x y)
deriving instance CoComplete (c x (w,y)) => CoComplete (Writer w c x y)
deriving instance UpperBounded (c x (w,y)) => UpperBounded (Writer w c x y)
