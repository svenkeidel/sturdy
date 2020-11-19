{-# LANGUAGE Arrows #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Fix.Context where

import Prelude hiding ((.))
import Control.Arrow hiding (ArrowMonad)
import Control.Arrow.Monad
import Control.Arrow.Fix
import Control.Arrow.Trans
import Control.Arrow.Transformer.Const
import Control.Arrow.Transformer.Kleisli
import Control.Arrow.Transformer.Reader
import Control.Arrow.Transformer.State
import Control.Arrow.Transformer.Static
import Control.Arrow.Transformer.Writer

import Data.Profunctor
import Data.Monoidal
import Data.Abstract.CallString as CallString

class (Arrow c, Profunctor c) => ArrowContext ctx a c | c -> a, c -> ctx where
  type Widening c
  joinByContext :: (?contextWidening :: Widening c) => c (ctx,a) a

  default joinByContext :: (c ~ t c', ArrowTrans t, ArrowContext ctx a c', ?contextWidening :: Widening c') => c (ctx,a) a
  joinByContext = lift' joinByContext
  {-# INLINE joinByContext #-}

class (Arrow c, Profunctor c) => ArrowCallSite label c | c -> label where
  getCallSite :: c () (CallString label)
  pushLabel :: Int -> c x y -> c (label, x) y

  default getCallSite :: (c ~ t c', ArrowTrans t, ArrowCallSite label c') => c () (CallString label)
  getCallSite = lift' getCallSite
  {-# INLINE getCallSite #-}

callsiteSensitive :: (?contextWidening :: Widening c,
                     ArrowCallSite lab c,
                     ArrowContext (CallString lab) a c)
                  => Int -> (a -> lab) -> FixpointCombinator c a b
callsiteSensitive k getLabel = callsiteSensitive' k getLabel
{-# INLINE callsiteSensitive #-}

callsiteSensitive' :: (?contextWidening :: Widening c,
                       ArrowCallSite lab c,
                       ArrowContext (CallString lab) a c)
                   => Int -> (a -> lab) -> FixpointCombinator c a b
callsiteSensitive' k getLabel f = recordCallSite k getLabel $ proc a -> do
  callSite <- getCallSite -< ()
  a' <- joinByContext -< (callSite, a)
  f -< a'
{-# INLINE callsiteSensitive' #-}

recordCallSite :: (ArrowCallSite lab c) => Int -> (a -> lab) -> FixpointCombinator c a b
recordCallSite k getLabel f = proc a -> do
  pushLabel k f -< (getLabel a, a)
{-# INLINE recordCallSite #-}


------------- Instances --------------
instance ArrowCallSite label c => ArrowCallSite label (ConstT r c) where
  pushLabel k f = lift $ \r -> pushLabel k (unlift f r)
  {-# INLINE pushLabel #-}

instance (ArrowMonad f c, ArrowCallSite callSite c) => ArrowCallSite callSite (KleisliT f c) where
  pushLabel k f = lift $ pushLabel k (unlift f)
  {-# INLINE pushLabel #-}

instance ArrowCallSite label c => ArrowCallSite label (ReaderT r c) where
  pushLabel k f = lift $ lmap shuffle1 (pushLabel k (unlift f))
  {-# INLINE pushLabel #-}

instance ArrowContext ctx a c => ArrowContext ctx a (ReaderT r c) where
  type Widening (ReaderT r c) = Widening c

instance ArrowCallSite label c => ArrowCallSite label (StateT s c) where
  pushLabel k f = lift $ lmap shuffle1 (pushLabel k (unlift f))
  {-# INLINE pushLabel #-}

instance ArrowContext ctx a c => ArrowContext ctx a (StateT s c) where
  type Widening (StateT s c) = Widening c

instance (Applicative f, ArrowCallSite label c) => ArrowCallSite label (StaticT f c) where
  pushLabel k (StaticT f) = StaticT $ pushLabel k <$> f
  {-# INLINE pushLabel #-}

instance (Monoid w, ArrowCallSite label c) => ArrowCallSite label (WriterT w c) where
  pushLabel k f = lift $ pushLabel k (unlift f)
  {-# INLINE pushLabel #-}

instance (Monoid w, ArrowContext ctx a c) => ArrowContext ctx a (WriterT w c) where
  type Widening (WriterT w c) = Widening c
