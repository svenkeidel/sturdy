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
import           Control.Arrow.Except
import           Control.Arrow.Trans
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.Random
import           Control.Arrow.State
import           Control.Arrow.Store

import           Control.Arrow.Transformer.State

import           System.Random(StdGen,Random)
import qualified System.Random as R

newtype RandomT c x y = RandomT (StateT StdGen c x y)

runRandomT :: RandomT c x y -> c (StdGen,x) (StdGen,y)
runRandomT (RandomT (StateT f)) = f

instance (Random v, Arrow c) => ArrowRand v (RandomT c) where
  random = RandomT $ proc () -> do
    gen <- get -< ()
    let (v,gen') = R.random gen
    put -< gen'
    returnA -< v

deriving instance Arrow c => Category (RandomT c)
deriving instance Arrow c => Arrow (RandomT c)
deriving instance ArrowChoice c => ArrowChoice (RandomT c)
deriving instance ArrowTrans RandomT
deriving instance ArrowLift RandomT
deriving instance ArrowReader r c => ArrowReader r (RandomT c)
deriving instance ArrowFail e c => ArrowFail e (RandomT c)
deriving instance ArrowExcept e c => ArrowExcept e (RandomT c)
deriving instance ArrowEnv var val env c => ArrowEnv var val env (RandomT c)
deriving instance ArrowAlloc x y c => ArrowAlloc x y (RandomT c)
deriving instance ArrowCond val c => ArrowCond val (RandomT c)
deriving instance ArrowStore var val c => ArrowStore var val (RandomT c)
deriving instance (Arrow c, ArrowFix (StdGen,x) (StdGen,y) c) => ArrowFix x y (RandomT c)

instance ArrowState s c => ArrowState s (RandomT c) where
  get = lift' get
  put = lift' put
