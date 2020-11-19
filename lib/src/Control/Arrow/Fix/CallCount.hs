{-# LANGUAGE Arrows #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Fix.CallCount where

import Prelude hiding ((.))
import Control.Arrow hiding (ArrowMonad)
import Control.Arrow.Monad
import Control.Arrow.Fix
import Control.Arrow.Fix.Context
import Control.Arrow.Trans
import Control.Arrow.Transformer.Const
import Control.Arrow.Transformer.Kleisli
import Control.Arrow.Transformer.Reader
import Control.Arrow.Transformer.State
import Control.Arrow.Transformer.Static
import Control.Arrow.Transformer.Writer

import Data.Profunctor
import Data.Monoidal

class (Arrow c, Profunctor c) => ArrowCallCount a c | c -> a where
  getCallCount :: c a Int
  incrementCallCount :: c x y -> c (a,x) y

  default getCallCount :: (c ~ t c', ArrowTrans t, ArrowCallCount a c') => c a Int
  getCallCount = lift' getCallCount
  {-# INLINE getCallCount #-}

unroll :: (?contextWidening :: Widening c, ArrowChoice c,
           ArrowCallCount label c, ArrowContext label a c)
       => Int -> (a -> label) -> FixpointCombinator c a b
unroll k getLabel f = proc a -> do
  let lab = getLabel a
  count <- getCallCount -< lab
  if count < k
    then incrementCallCount f -< (lab, a)
    else do
      a' <- joinByContext -< (lab, a)
      incrementCallCount f -< (lab, a')
{-# INLINE unroll #-}

------------- Instances --------------
instance ArrowCallCount label c => ArrowCallCount label (ConstT r c) where
  incrementCallCount f = lift $ \r -> incrementCallCount (unlift f r)
  {-# INLINE incrementCallCount #-}

instance (ArrowMonad f c, ArrowCallCount callSite c) => ArrowCallCount callSite (KleisliT f c) where
  incrementCallCount f = lift $ incrementCallCount (unlift f)
  {-# INLINE incrementCallCount #-}

instance ArrowCallCount label c => ArrowCallCount label (ReaderT r c) where
  incrementCallCount f = lift $ lmap shuffle1 (incrementCallCount (unlift f))
  {-# INLINE incrementCallCount #-}

instance ArrowCallCount label c => ArrowCallCount label (StateT s c) where
  incrementCallCount f = lift $ lmap shuffle1 (incrementCallCount (unlift f))
  {-# INLINE incrementCallCount #-}

instance (Applicative f, ArrowCallCount label c) => ArrowCallCount label (StaticT f c) where
  incrementCallCount (StaticT f) = StaticT $ incrementCallCount <$> f
  {-# INLINE incrementCallCount #-}

instance (Monoid w, ArrowCallCount label c) => ArrowCallCount label (WriterT w c) where
  incrementCallCount f = lift $ incrementCallCount (unlift f)
  {-# INLINE incrementCallCount #-}
