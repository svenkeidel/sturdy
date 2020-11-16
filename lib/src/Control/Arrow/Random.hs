{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Random where

import Control.Arrow
import Control.Arrow.Trans
import Control.Arrow.Transformer.State
import Control.Arrow.Transformer.Writer
import Data.Profunctor

class (Arrow c, Profunctor c) => ArrowRand v c where
  random :: c () v

  default random :: (c ~ t c', ArrowTrans t, ArrowRand v c') => c () v
  random = lift' random

------------- Instances --------------
instance ArrowRand v c => ArrowRand v (StateT s c) where
  random = lift' random
  {-# INLINE random #-}

instance (Monoid w, ArrowRand v c) => ArrowRand v (WriterT w c) where
  random = lift' random
  {-# INLINE random #-}
