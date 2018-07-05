{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Concrete.Random where

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Alloc
import           Control.Arrow.Conditional
import           Control.Arrow.Environment
import           Control.Arrow.Lift
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.Random
import           Control.Arrow.State
import           Control.Arrow.Store

import           Control.Arrow.Transformer.State

import           System.Random(StdGen)
import qualified System.Random as R

newtype Random c x y = Random (State StdGen c x y)

runRandom :: Random c x y -> c (StdGen,x) (StdGen,y)
runRandom (Random (State f)) = f

instance (R.Random v, Arrow c) => ArrowRand v (Random c) where
  random = Random $ proc () -> do
    gen <- get -< ()
    let (v,gen') = R.random gen
    put -< gen'
    returnA -< v

deriving instance Category c => Category (Random c)
deriving instance Arrow c => Arrow (Random c)
deriving instance ArrowChoice c => ArrowChoice (Random c)
deriving instance ArrowLift Random
deriving instance ArrowReader r c => ArrowReader r (Random c)
deriving instance ArrowFail e c => ArrowFail e (Random c)
deriving instance ArrowEnv x y env c => ArrowEnv x y env (Random c)
deriving instance ArrowAlloc x y c => ArrowAlloc x y (Random c)
deriving instance ArrowCond val (StdGen,x) (StdGen,y) (StdGen,z) c => ArrowCond val x y z (Random c)
deriving instance ArrowRead var val (StdGen,x) (StdGen,y) c => ArrowRead var val x y (Random c)
deriving instance ArrowWrite var val c => ArrowWrite var val (Random c)

type instance Fix x y (Random c) = Random (Fix (StdGen,x) (StdGen,y) c)
deriving instance (Arrow c, ArrowFix (StdGen,x) (StdGen,y) c) => ArrowFix x y (Random c)

instance ArrowState s c => ArrowState s (Random c) where
  get = lift get
  put = lift put
