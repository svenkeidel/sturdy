{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Fix.Context where

import Prelude hiding ((.))
import Control.Category
import Control.Arrow
import Control.Arrow.Fix
import Control.Arrow.Trans

import Data.Profunctor
import Data.Abstract.CallString as CallString

class (Arrow c, Profunctor c) => ArrowContext ctx c | c -> ctx where
  askContext :: c () ctx
  localContext :: c x y -> c (ctx,x) y

  default askContext :: (c ~ t c', ArrowLift t, ArrowContext ctx c') => c () ctx
  askContext = lift' askContext
  {-# INLINE askContext #-}

class (Arrow c, Profunctor c) => ArrowJoinContext a c | c -> a where
  joinByContext :: c a a

  default joinByContext :: (c ~ t c', ArrowLift t, ArrowJoinContext a c') => c a a
  joinByContext = lift' joinByContext
  {-# INLINE joinByContext #-}

callsiteSensitive :: forall a lab b c. (ArrowContext (CallString lab) c, ArrowJoinContext a c) => Int -> (a -> lab) -> FixpointCombinator c a b
callsiteSensitive k getLabel = callsiteSensitive' k (Just . getLabel)
{-# INLINE callsiteSensitive #-}

callsiteSensitive' :: forall a lab b c. (ArrowContext (CallString lab) c, ArrowJoinContext a c) => Int -> (a -> Maybe lab) -> FixpointCombinator c a b
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
