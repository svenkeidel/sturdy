{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Fix.Context where

import           Prelude hiding (lookup,truncate,(.),id)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Primitive
import           Control.Arrow.Strict
import           Control.Arrow.Fix.Context
import           Control.Arrow.Fix.ControlFlow
import           Control.Arrow.Trans

import           Control.Arrow.Transformer.State

import           Data.Coerce
import           Data.Empty
import qualified Data.HashMap.Strict as M
import           Data.HashMap.Strict (HashMap)
import           Data.Identifiable
import           Data.Maybe(fromMaybe)
import           Data.Order
import           Data.Profunctor.Unsafe

import qualified Data.Abstract.Widening as W

newtype ContextT cache ctx a c x y = ContextT (StateT (cache ctx a) c x y)
  deriving (Category,Profunctor,Arrow,ArrowChoice,ArrowStrict,
            ArrowLift,ArrowControlFlow stmt, ArrowPrimitive, ArrowCFG graph)

instance ArrowTrans (ContextT cache ctx a) where
  lift' = ContextT . lift'
  {-# INLINE lift' #-}

instance (IsEmpty (cache ctx a), ArrowRun c) => ArrowRun (ContextT cache ctx a c) where
  type Run (ContextT cache ctx a c) x y = Run c x y
  run (ContextT f) = run (dimap (empty,) snd (runStateT f))
  {-# INLINE run #-}

instance (Profunctor c,ArrowApply c) => ArrowApply (ContextT cache ctx a c) where
  app = ContextT (app .# first coerce)
  {-# INLINE app #-}

------ Context Cache ------
newtype Context ctx a = Context (HashMap ctx a)

instance IsEmpty (Context ctx a) where
  empty = Context empty

instance (Show ctx, Show a) => Show (Context ctx a) where
  show (Context m) = show (M.toList m)

instance (Identifiable ctx, PreOrd a, Profunctor c, ArrowChoice c)
      => ArrowContext ctx a (ContextT Context ctx a c) where

  type Widening (ContextT Context ctx a c) = W.Widening a

  joinByContext = lift $ proc (Context cache, (ctx,a)) -> do
    returnA -< case M.lookup ctx cache of
      -- If there exists a stable cached entry and the actual input is
      -- smaller than the cached input, reuse the cached input.
      Just a'
        | a ⊑ a' -> (Context cache, a')
        | otherwise ->
          -- If the actual input is not smaller than the cached
          -- input, widen the input.
          let (_,a'') = ?contextWidening a' a
          in (Context (M.insert ctx a'' cache),a'')
      Nothing -> (Context (M.insert ctx a cache),a)
  {-# INLINE joinByContext #-}

------ Cache that does not join the first argument ------
data Second cache ctx a where
  Second :: cache ctx b -> Second cache ctx (a,b)

instance IsEmpty (cache ctx b) => IsEmpty (Second cache ctx (a,b)) where
  empty = Second empty

instance (ArrowContext ctx b (ContextT cache ctx b c), Arrow c, Profunctor c)
       => ArrowContext ctx (a,b) (ContextT (Second cache) ctx (a, b) c) where

  type Widening (ContextT (Second cache) ctx (a,b) c) = Widening (ContextT cache ctx b c)

  joinByContext = proc (ctx, (a, b)) -> do
    b' <- liftSecond joinByContext -< (ctx, b)
    returnA -< (a,b')
  {-# INLINE joinByContext #-}

liftSecond :: Profunctor c => ContextT cache ctx b c x y -> ContextT (Second cache) ctx (a, b) c x y
liftSecond f =
  lift $ dimap (\(Second cache, x) -> (cache, x))
               (\(cache, y) -> (Second cache, y))
               (unlift f)
{-# INLINE liftSecond #-}

------ Group Cache ------
data Group cache ctx a where
  Group :: HashMap k (cache ctx a) -> Group cache ctx (k,a)

instance IsEmpty (Group cache ctx (k,a)) where
  empty = Group empty

instance (ArrowContext ctx a (ContextT cache ctx a c), Arrow c, Profunctor c, Identifiable k, IsEmpty (cache ctx a))
       => ArrowContext ctx (k,a) (ContextT (Group cache) ctx (k, a) c) where

  type Widening (ContextT (Group cache) ctx (k,a) c) = Widening (ContextT cache ctx a c)

  joinByContext = proc (ctx,(k,a)) -> do
    a' <- withGroup joinByContext -< (k,(ctx,a))
    returnA -< (k,a')
  {-# INLINE joinByContext #-}

withGroup :: (Identifiable k, IsEmpty (cache ctx a), Profunctor c, Arrow c)
          => ContextT cache ctx a c x y -> ContextT (Group cache) ctx (k,a) c (k,x) y
withGroup f = lift $
  dimap (\(Group groups,(k,x)) -> ((groups,k),(fromMaybe empty (M.lookup k groups),x)))
        (\((groups,k),(cache,y)) -> (Group (M.insert k cache groups), y))
        (second (unlift f))
{-# INLINE withGroup #-}
