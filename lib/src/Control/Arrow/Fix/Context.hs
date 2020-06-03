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
import Control.Category
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

class (Arrow c, Profunctor c) => ArrowContext ctx c | c -> ctx where
  askContext :: c () ctx
  localContext :: c x y -> c (ctx,x) y

  default askContext :: (c ~ t c', ArrowLift t, ArrowContext ctx c') => c () ctx
  askContext = lift' askContext
  {-# INLINE askContext #-}

class (Arrow c, Profunctor c) => ArrowJoinContext a c | c -> a where
  type Widening c

  joinByContext :: (?contextWidening :: Widening c) => c a a

  default joinByContext :: (c ~ t c', ArrowLift t, ArrowJoinContext a c', ?contextWidening :: Widening c') => c a a
  joinByContext = lift' joinByContext
  {-# INLINE joinByContext #-}

callsiteSensitive :: forall a lab b c. (?contextWidening :: Widening c, ArrowContext (CallString lab) c, ArrowJoinContext a c) => Int -> (a -> lab) -> FixpointCombinator c a b
callsiteSensitive k getLabel = callsiteSensitive' k (Just . getLabel)
{-# INLINE callsiteSensitive #-}

callsiteSensitive' :: forall a lab b c. (?contextWidening :: Widening c, ArrowContext (CallString lab) c, ArrowJoinContext a c) => Int -> (a -> Maybe lab) -> FixpointCombinator c a b
callsiteSensitive' k getLabel f = recordCallsite k getLabel $ f . joinByContext
{-# INLINE callsiteSensitive' #-}

recordCallsite :: forall a lab b c. ArrowContext (CallString lab) c => Int -> (a -> Maybe lab) -> FixpointCombinator c a b
recordCallsite k getLabel g = proc a -> do
  callString <- askContext -< ()
  let callString' = case getLabel a of
        Just lab -> CallString.truncate k (CallString.push lab callString)
        Nothing -> callString
  localContext g -< (callString',a)
{-# INLINE recordCallsite #-}

------------- Instances --------------
instance ArrowContext ctx c => ArrowContext ctx (ConstT r c) where
  localContext f = lift $ \r -> localContext (unlift f r)
  {-# INLINE localContext #-}

instance (ArrowMonad f c, ArrowContext ctx c) => ArrowContext ctx (KleisliT f c) where
  localContext (KleisliT f) = KleisliT (localContext f)
  {-# INLINE localContext #-}

instance ArrowContext ctx c => ArrowContext ctx (ReaderT r c) where
  localContext f = lift $ lmap shuffle1 (localContext (unlift f))
  {-# INLINE localContext #-}

instance ArrowJoinContext a c => ArrowJoinContext a (ReaderT r c) where
  type Widening (ReaderT r c) = Widening c

instance ArrowContext ctx c => ArrowContext ctx (StateT s c) where
  localContext f = lift (lmap shuffle1 (localContext (unlift f)))
  {-# INLINE localContext #-}

instance ArrowJoinContext a c => ArrowJoinContext a (StateT s c) where
  type Widening (StateT s c) = Widening c

instance (Applicative f, ArrowContext ctx c) => ArrowContext ctx (StaticT f c) where
  localContext (StaticT f) = StaticT $ localContext <$> f
  {-# INLINE localContext #-}
  {-# SPECIALIZE instance ArrowContext ctx c => ArrowContext ctx (StaticT ((->) r) c) #-}

instance (Monoid w, ArrowContext ctx c) => ArrowContext ctx (WriterT w c) where
  localContext f = lift (localContext (unlift f))
  {-# INLINE localContext #-}

instance (Monoid w, ArrowJoinContext a c) => ArrowJoinContext a (WriterT w c) where
  type Widening (WriterT w c) = Widening c
