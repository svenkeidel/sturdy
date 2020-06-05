{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Strict where

import           Control.Arrow
import           Control.Arrow.Trans
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Static

import           Control.DeepSeq (NFData)
import qualified Control.DeepSeq as DeepSeq

import           Data.Profunctor

-- | Evaluate the result of a computation to weak head normal form.
--
-- Due to ConcurrentUtils: Control.CUtils.StrictArrow
class (Profunctor c, Arrow c) => ArrowStrict c where
  force :: c a b -> c a b

  default force :: (ArrowLift c, Underlying c a b ~ c' a' b', ArrowStrict c') => c a b -> c a b
  force = lift1 force
  {-# INLINE force #-}

instance ArrowStrict (->) where
  force f x = x `seq` f x

-- | Reduce the return value to normal form.
reduce :: (NFData b, ArrowStrict c) => c a b -> c a b
reduce f = force (rmap DeepSeq.force f)
{-# INLINE reduce #-}

------------- Instances --------------
instance (ArrowStrict c) => ArrowStrict (ConstT r c) where
  force f = lift $ \r -> force (unlift f r)
  {-# INLINE force #-}

instance ArrowStrict c => ArrowStrict (ReaderT r c)
instance ArrowStrict c => ArrowStrict (StateT s c)

instance (Applicative f, ArrowStrict c) => ArrowStrict (StaticT f c) where
  force (StaticT f) = StaticT $ force <$> f
  {-# INLINE force #-}
  {-# SPECIALIZE instance ArrowStrict c => ArrowStrict (StaticT ((->) r) c) #-}
