{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Fix.Combinator
( FixpointCombinator
, iterateInner
, iterateOuter
, transform
, filter
, reuseFirst
, reuseExact
, reuseByMetric
, reuseStableByMetric
, maxSize
, widenInput
, callsiteSensitive
, callsiteSensitive'
, recordCallsite
, trace
, trace'
, traceCache
, traceCtx
, traceShow
)
where

import           Prelude hiding ((.),pred,lookup,filter,head,iterate,map)

import           Control.Category
import           Control.Arrow hiding (loop)
import           Control.Arrow.State(ArrowState)
import qualified Control.Arrow.State as State
import           Control.Arrow.Fix.Stack (ArrowStack)
import qualified Control.Arrow.Fix.Stack as Stack
import           Control.Arrow.Fix.Context (ArrowContext,ArrowJoinContext)
import qualified Control.Arrow.Fix.Context as Ctx
import           Control.Arrow.Fix.Reuse as Reuse
import           Control.Arrow.Fix.Cache as Cache
import           Control.Arrow.Fix.Chaotic
import           Control.Arrow.Utils (map)

import           Data.Abstract.Widening as W
import           Data.Abstract.Stable
import           Data.Abstract.CallString as CallString
import           Data.Order
import           Data.Metric
import           Data.Monoid (First(..))
import           Data.Profunctor
import           Data.Lens (Iso',from,Prism',getMaybe,get,set)
import           Data.Identifiable
import qualified Data.HashSet as H

import           Text.Printf
import qualified Debug.Trace as Debug

type FixpointCombinator c x y = c x y -> c x y

-- | Iterate on the innermost fixpoint component.
iterateInner :: forall a b c. (Identifiable a, ArrowStack a c, ArrowChaotic a c, ArrowCache a b c, ArrowChoice c) => FixpointCombinator c a b
{-# INLINE iterateInner #-}
iterateInner = detectLoop . go
  where
    go f = withComponent f $ proc (a,b,component) ->
      -- The call did not depend on any unstable calls. This means
      -- we are done and don't need to iterate.
      if H.null (head component)
      then do
        Cache.write -< (a,b,Stable)
        returnA -< b
      else do
        (stable,bNew) <- Cache.update -< (a,b)
        case stable of
          Stable ->
            if head component == H.singleton a
            then do
              map Cache.setStable -< (Stable,) <$> H.toList (body component)
              setComponent -< (mempty, bNew)
            else do
              setStable    -< (Unstable,a)
              setComponent -< (component { head = H.delete a (head component)
                                         , body = H.insert a (body component) }, bNew)
          Unstable -> go f -< a

-- | Iterate on the outermost fixpoint component.
iterateOuter :: forall a b c. (Identifiable a, ArrowStack a c, ArrowChaotic a c, ArrowCache a b c, ArrowChoice c) => FixpointCombinator c a b
{-# INLINE iterateOuter #-}
iterateOuter = detectLoop . go
  where
    go f = withComponent f $ proc (a,b,component) -> case () of
      -- The call did not depend on any unstable calls. This means
      -- we are done and don't need to iterate.
      () | H.null (head component) -> do
           Cache.write -< (a,b,Stable)
           setComponent -< (mempty,b)

      -- We are at the head of a fixpoint component. This means, we
      -- have to iterate until the head stabilized.
         | head component == H.singleton a -> do
           (stable,bNew) <- Cache.update -< (a,b)

           case stable of
             -- If the head of a fixpoint component is stable, set
             -- all elements in the body of the component as stable
             -- too and return.
             Stable -> do
               map Cache.setStable -< H.toList $ H.map (Stable,) (body component)
               setComponent -< (mempty, bNew)

             -- If the head of a fixpoint component is not stable, keep iterating.
             Unstable ->
               go f -< a

      -- We are inside an  fixpoint component, but its head has not stabilized.
         | otherwise -> do
           Cache.write -< (a,b,Unstable)
           setComponent -< (Component { head = H.delete a (head component),
                                        body = H.insert a (body component) }, b)

detectLoop :: (ArrowStack a c, ArrowCache a b c, ArrowChaotic a c, ArrowChoice c) => FixpointCombinator c a b
detectLoop f = proc a -> do
  loop <- Stack.elem -< a
  if loop
  then do
    m <- Cache.lookup -< a
    case m of
      Just (Stable,b) -> returnA -< b
      Just (Unstable,b) -> iterate -< (a, b)
      Nothing -> do
        b <- initialize -< a
        iterate -< (a, b)
  else Stack.push f -< a
{-# INLINE detectLoop #-}

transform :: Profunctor c => Iso' a a' -> FixpointCombinator c a' b -> FixpointCombinator c a b
transform iso strat f = lmap (get iso)
                      $ strat
                      $ lmap (get (from iso)) f
{-# INLINE transform #-}

filter :: forall a a' b c. (Profunctor c, ArrowChoice c, ArrowApply c) => Prism' a a' -> FixpointCombinator c a' b -> FixpointCombinator c a b
filter pred strat f = proc a -> case getMaybe pred a of
  Just a' -> strat (lmap (\x -> set pred x a) f) -<< a'
  Nothing -> f -< a
{-# INLINE filter #-}

reuseFirst :: (PreOrd a, ArrowChoice c, ArrowReuse a b c) => Stable -> FixpointCombinator c a b
reuseFirst s f = proc a -> do
  m <- reuse s (\a a' s' b' m -> case m of
                 First (Just _) -> m
                 First Nothing
                   | a ⊑ a' -> First (Just (a',b',s'))
                   | otherwise -> m) -< a
  case getFirst m of
    Just (_,b,Stable) -> returnA -< b
    Just (a',_,Unstable) -> f -< a'
    Nothing -> f -< a
{-# INLINE reuseFirst #-}

reuseExact :: (ArrowChoice c, ArrowCache a b c) => FixpointCombinator c a b
reuseExact f = proc a -> do
  m <- lookup -< a
  case m of
    Just (Stable,b) -> returnA -< b
    _ -> f -< a
{-# INLINE reuseExact #-}

reuseByMetric :: (PreOrd a, Ord n, ArrowChoice c, ArrowReuse a b c) => Metric a n -> FixpointCombinator c a b
reuseByMetric metric = reuseByMetric_ (\s a a' -> Product s (metric a a')) Unstable
{-# INLINE reuseByMetric #-}

reuseStableByMetric :: (PreOrd a, Ord n, ArrowChoice c, ArrowReuse a b c) => Metric a n -> FixpointCombinator c a b
reuseStableByMetric metric = reuseByMetric_ (const metric) Stable
{-# INLINE reuseStableByMetric #-}

reuseByMetric_ :: (PreOrd a, Ord n, ArrowChoice c, ArrowReuse a b c) => (Stable -> Metric a n) -> Stable -> FixpointCombinator c a b
reuseByMetric_ metric s f = proc a -> do
  m <- reuse s (\a a' s' b' m ->
                if a ⊑ a'
                then m <> Just (Measured { input = a', output = b', isStable = s', measured = metric s' a a' })
                else m) -< a
  case m of
    Just Measured { isStable = Stable, output = b } -> returnA -< b
    Just Measured { isStable = Unstable, input = a' } -> f -< a'
    Nothing -> f -< a
{-# INLINE reuseByMetric_ #-}

data Measured a b n = Measured { input :: a, output :: b, isStable :: Stable, measured :: n }

instance (Show a, Show b, Show n) => Show (Measured a b n) where
  show m = printf "%s@%s" (show (output m)) (show (measured m))

instance Ord n => Semigroup (Measured a b n) where
  m1 <> m2
    | measured m1 <= measured m2 = m1
    | otherwise                  = m2
  {-# INLINE (<>) #-}

maxSize :: (ArrowChoice c, ArrowStack a c) => Int -> FixpointCombinator c a b -> FixpointCombinator c a b
maxSize limit strat f = proc a -> do
  n <- Stack.size -< ()
  if n < limit
  then f -< a
  else strat f -< a
{-# INLINE maxSize #-}

widenInput :: (Complete a, ArrowStack a c) => W.Widening a -> FixpointCombinator c a b
widenInput widen f = proc a -> do
  m <- Stack.peek -< ()
  f -< case m of
    Nothing -> a
    Just x  -> snd $ x `widen` (x ⊔ a)
{-# INLINE widenInput #-}

callsiteSensitive :: forall a lab b c. (ArrowContext (CallString lab) c, ArrowJoinContext a c) => Int -> (a -> lab) -> FixpointCombinator c a b
callsiteSensitive k getLabel = callsiteSensitive' k (Just . getLabel)
{-# INLINE callsiteSensitive #-}

callsiteSensitive' :: forall a lab b c. (ArrowContext (CallString lab) c, ArrowJoinContext a c) => Int -> (a -> Maybe lab) -> FixpointCombinator c a b
callsiteSensitive' k getLabel f = recordCallsite k getLabel $ f . Ctx.joinByContext
{-# INLINE callsiteSensitive' #-}

recordCallsite :: forall a lab b c. ArrowContext (CallString lab) c => Int -> (a -> Maybe lab) -> FixpointCombinator c a b
recordCallsite k getLabel g = proc a -> do
  callString <- Ctx.askContext -< ()
  let callString' = case getLabel a of
        Just lab -> CallString.truncate k (CallString.push lab callString)
        Nothing -> callString
  Ctx.localContext g -< (callString',a)
{-# INLINE recordCallsite #-}

trace :: (Arrow c) => (a -> String) -> (b -> String) -> FixpointCombinator c a b
trace showA showB f = proc x -> do
  y <- f -< Debug.trace (printf "CALL\n%s\n\n" (showA x)) x
  returnA -< Debug.trace (printf "RETURN\n%s\n%s\n\n" (showA x) (showB y)) y
{-# INLINE trace #-}

trace' :: (Eq a, ArrowApply c) => (a -> String) -> (b -> String) -> FixpointCombinator c a b -> FixpointCombinator c a b
trace' showA showB strat f = proc x -> do
  y <- strat (proc x' -> f -< Debug.trace (if x == x'
                                           then printf "CALL\n%s\n\n" (showA x)
                                           else printf "CALL\n%s~>\n%s\n\n" (showA x) (showA x')) x') -<< x
  returnA -< Debug.trace (printf "RETURN\n%s\n%s\n\n" (showA x) (showB y)) y
{-# INLINE trace' #-}

traceCache :: ArrowState cache c => (cache -> String) -> FixpointCombinator c a b
traceCache showCache f = proc a -> do
  cache <- State.get -< ()
  f -< Debug.trace (printf "CACHE %s\n\n" (showCache cache)) a
{-# INLINE traceCache #-}

traceCtx :: (ArrowContext ctx a' c,ArrowState cache c) => (a -> String) -> (b -> String) -> (ctx -> String) -> (cache -> String) -> FixpointCombinator c a b
traceCtx showA showB showCtx showCache f = proc x -> do
  ctx <- Ctx.askContext -< ()
  cache <- State.get -< ()
  y <- f -< Debug.trace (printf "CALL\n%s\n%s\n%s\n\n" (showA x) (showCtx ctx) (showCache cache)) x
  returnA -< Debug.trace (printf "RETURN\n%s\n%s\n%s\n\n" (showA x) (showCtx ctx) (showB y)) y
{-# INLINE traceCtx #-}

traceShow :: (Show a, Show b, Arrow c) => FixpointCombinator c a b
traceShow = trace show show
{-# INLINE traceShow #-}
