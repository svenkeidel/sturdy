{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Abstract.Fix.Chaotic where

import           Prelude hiding (pred,lookup,map,head,iterate,(.))

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.Trans
import           Control.Arrow.Order(ArrowComplete(..),ArrowJoin(..))
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Writer

import           Data.Profunctor
import           Data.Order
import           Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.HashSet(HashSet)
import qualified Data.HashSet as H
import           Data.Identifiable
import           Data.Empty
import           Data.Coerce

import           Data.Abstract.StackWidening(Stack(..))
import           Data.Abstract.Widening(Widening,Stable(..))

newtype ChaoticT a b c x y =
  ChaoticT (
    WriterT (Component a)
      (StateT (HashMap a (b,Stable))
        (ReaderT (Stack a) c)) x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice)

runChaoticT :: Profunctor c => ChaoticT a b c x y -> c x y
runChaoticT f = rmap snd (runChaoticT' f)

runChaoticT' :: Profunctor c => ChaoticT a b c x y -> c x (HashMap a (b,Stable),y)
runChaoticT' (ChaoticT f) = dimap (\a -> (empty,(M.empty,a))) (second snd) (runReaderT (runStateT (runWriterT f)))

chaotic :: (Identifiable a, LowerBounded b, Profunctor c, ArrowChoice c, ArrowApply c) => Widening b -> IterationStrategy (ChaoticT a b c) a b
chaotic widen (ChaoticT (WriterT (StateT f))) = ChaoticT $ WriterT $ StateT $ push $ proc (stack,cache,a) -> do
  case M.lookup a cache of
    Just (b,Stable) ->
      returnA -< (cache,(mempty,b))

    Just (b,Instable) | H.member a stack ->
      returnA -< (cache,(Component {head = H.singleton a, body = H.empty},b))

    _ ->
      iterate -<< (M.insertWith (\_ o -> o) a (bottom,Instable) cache,a)

  where
    iterate = proc (cache,a) -> do
      (cache',(component,b)) <- f -< (cache,a)

      case () of
        -- The call did not depend on any unstable calls. This means we are done and don't need to iterate.
        () | H.null (head component) -> do
             returnA -< (M.insert a (b,Stable) cache',(mempty,b))

        -- We are at the head of a fixpoint component. This means, we have to iterate until the head stabilized.
           | head component == H.singleton a -> do
             -- Cache.update a b cache
             let (bOld,_) = M.lookupDefault (bottom,Instable) a cache
                 (stable,bNew) = widen bOld b
             let cache'' = M.insert a (bNew,stable) cache'

             case stable of
               -- If the head of a fixpoint component is stable, flag all elements in the body of the component as stable too and return.
               Stable -> do
                 returnA -< (foldr (M.adjust (second (\_ -> Stable))) cache'' (body component),(mempty,bNew))

               -- If the head of a fixpoint component is not stable, keep iterating.
               Instable ->
                 iterate -<< (cache'',a)

        -- We are in an unstable fixpoint component, but not at its head. This means, we have to wait until the head stabilized.
           | otherwise -> do
             returnA -< (M.insert a (b,Instable) cache,(Component {head = H.delete a (head component), body = H.insert a (body component)}, b)) 

    push g = proc (cache,a) -> do
      Stack xs <- ask -< ()
      local g -< (Stack (H.insert a xs),(xs,cache,a))

instance (Identifiable a, ArrowRun c) => ArrowRun (ChaoticT a b c) where
  type Rep (ChaoticT a b c) x y = Rep c x y
  run = run . runChaoticT

instance (Identifiable a,Profunctor c,ArrowApply c) => ArrowApply (ChaoticT a b c) where app = ChaoticT (lmap (first coerce) app)
instance (Identifiable a,Profunctor c,Arrow c) => ArrowJoin (ChaoticT a b c) where
  join _lub (ChaoticT f) (ChaoticT g) = ChaoticT $ rmap (uncurry _lub) (f &&& g)
instance (Identifiable a,Profunctor c,Arrow c, Complete y) => ArrowComplete y (ChaoticT a b c) where
  ChaoticT f <⊔> ChaoticT g = ChaoticT $ rmap (uncurry (⊔)) (f &&& g)

data Component a = Component { head :: HashSet a, body :: HashSet a }
instance Identifiable a => Semigroup (Component a) where (<>) = mappend
instance Identifiable a => Monoid (Component a) where
  mempty = Component { head = H.empty, body = H.empty }
  c1 `mappend` c2 = Component { head = head c1 <> head c2, body = body c1 <> body c2 }
