{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Arrow.Transformer.Abstract.Fix.Cache where

import Prelude hiding (pred,lookup,map,head,iterate,(.),id,truncate,elem,product,(**))

import Control.Category
import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Trans
import Control.Arrow.State
import Control.Arrow.Fix.Context as Context
import Control.Arrow.Fix.Cache as Cache
import Control.Arrow.Fix.Reuse as Reuse
import Control.Arrow.Order(ArrowJoin(..),ArrowComplete(..),ArrowEffectCommutative)

import Control.Arrow.Transformer.Const
import Control.Arrow.Transformer.Static
import Control.Arrow.Transformer.State

import Data.Profunctor.Unsafe
import Data.Empty
import Data.Order hiding (lub)
import Data.Coerce
import Data.Identifiable
import Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as M
import Data.Monoidal
import Data.Maybe

import Data.Abstract.Stable
import qualified Data.Abstract.Widening as W

import GHC.Exts

newtype CacheT cache a b c x y = CacheT { unCacheT :: ConstT (Cache.Widening (CacheT cache a b c)) (StateT (cache a b) c) x y}
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowState (cache a b))

instance ArrowTrans (CacheT cache a b c) where
  type Underlying (CacheT cache a b c) x y = Cache.Widening (CacheT cache a b c) -> c (cache a b, x) (cache a b, y)

runCacheT :: (IsEmpty (cache a b), Profunctor c) => Cache.Widening (CacheT cache a b c) -> CacheT cache a b c x y -> c x (cache a b,y)
runCacheT widen (CacheT f) = lmap (empty,) (runStateT (runConstT widen f))
{-# INLINE runCacheT #-}

liftCacheT :: (Cache.Widening (CacheT cache' a' b c) ~ Cache.Widening (CacheT cache a b c), Arrow c)
  => CacheT cache' a' b c x y -> CacheT cache a b c (cache' a' b,x) (cache' a' b,y)
liftCacheT (CacheT f) = CacheT (lift $ \widen -> withStateT (runConstT widen f))
{-# INLINE liftCacheT #-}

liftCacheT' :: (Cache.Widening (CacheT cache' a' b c) ~ Cache.Widening (CacheT cache a b c), Arrow c)
  => CacheT cache' a' b c x y -> ConstT (Cache.Widening (CacheT cache a b c)) (StateT (cache a b) c) (cache' a' b,x) (cache' a' b,y)
liftCacheT' = unCacheT . liftCacheT
{-# INLINE liftCacheT' #-}

instance (IsEmpty (cache a b), ArrowRun c) => ArrowRun (CacheT cache a b c) where
  type Run (CacheT cache a b c) x y = Cache.Widening (CacheT cache a b c) -> Run c x (cache a b,y)
  run f widen = run (runCacheT widen f)
  {-# INLINE run #-}

instance ArrowLift (CacheT cache a b) where
  lift' = CacheT . lift' . lift'
  {-# INLINE lift' #-}

instance (Complete y, ArrowEffectCommutative c) => ArrowComplete y (CacheT cache a b c) where
  CacheT f <⊔> CacheT g = CacheT $ rmap (uncurry (⊔)) (f &&& g)
  {-# INLINE (<⊔>) #-}

instance (Arrow c, Profunctor c) => ArrowJoin (CacheT cache a b c) where
  joinSecond lub f (CacheT g) = CacheT (rmap (\(x,y) -> f x `lub` y) (id &&& g))
  {-# INLINE joinSecond #-}

instance (Arrow c, ArrowContext ctx c) => ArrowContext ctx (CacheT cache a b c) where
  localContext (CacheT f) = CacheT (localContext f)
  {-# INLINE localContext #-}

instance (Profunctor c,ArrowApply c) => ArrowApply (CacheT cache a b c) where
  app = CacheT (app .# first coerce)
  {-# INLINE app #-}

instance ArrowEffectCommutative c => ArrowEffectCommutative (CacheT cache a b c)

----- Basic Cache -----
newtype Cache a b = Cache { getMap :: HashMap a (Stable,b)}

instance IsEmpty (Cache a b) where
  empty = Cache M.empty

instance (Show a, Show b) => Show (Cache a b) where
  show (Cache m) = show (M.toList m)

instance (Identifiable a, LowerBounded b, ArrowChoice c, Profunctor c) => ArrowCache a b (CacheT Cache a b c) where
  type Widening (CacheT Cache a b c) = W.Widening b
  initialize = CacheT $ proc _ -> returnA -< bottom
  lookup = CacheT $ proc a -> do
    Cache cache <- get -< ()
    returnA -< M.lookup a cache
  update = CacheT $ askConst $ \widen -> proc (a,b) -> do
    Cache cache <- get -< ()
    case M.lookup a cache of
      Just (_,b') -> do
        let b'' = widen b' b
        put -< Cache (M.insert a b'' cache)
        returnA -< b''
      Nothing -> do
        put -< Cache (M.insert a (Unstable,b) cache)
        returnA -< (Unstable,b)
  write = CacheT $ modify' (\((a,b,s),Cache cache) -> ((),Cache (M.insert a (s,b) cache)))
  setStable = CacheT $ modify' $ \((s,a),Cache cache) -> ((),Cache (M.adjust (first (const s)) a cache))
  {-# INLINE initialize #-}
  {-# INLINE lookup #-}
  {-# INLINE write #-}
  {-# INLINE update #-}
  {-# INLINE setStable #-}

instance (Arrow c, Profunctor c) => ArrowReuse a b (CacheT Cache a b c) where
  reuse s f = CacheT $ proc a -> do
    Cache cache <- get -< ()
    returnA -< M.foldlWithKey' (\m a' (s',b') -> if s' ⊑ s then f a a' s' b' m else m) mempty cache
  {-# INLINE reuse #-}

instance Identifiable a => IsList (Cache a b) where
  type Item (Cache a b) = (a,b,Stable)
  toList (Cache m) = [ (a,b,s) | (a,(s,b)) <- M.toList m]
  fromList l = Cache $ M.fromList [ (a,(s,b)) | (a,b,s) <- l]

------ Group Cache ------
data Group cache a b where
  Groups :: HashMap k (cache a b) -> Group cache (k,a) b

instance IsEmpty (Group cache (k,a) b) where
  empty = Groups empty
  {-# INLINE empty #-}

instance (Identifiable k, Arrow c, Profunctor c, ArrowCache a b (CacheT cache a b c), IsEmpty (cache a b)) => ArrowCache (k,a) b (CacheT (Group cache) (k,a) b c) where
  type Widening (CacheT (Group cache) (k,a) b c) = Cache.Widening (CacheT cache a b c)
  initialize = withCache Cache.initialize
  lookup = withCache Cache.lookup
  update = lmap assoc2 (withCache Cache.update)
  write = lmap (\((k,a),b,s) -> (k,(a,b,s))) (withCache Cache.write)
  setStable = lmap shuffle1 (withCache Cache.setStable)
  {-# INLINE initialize #-}
  {-# INLINE lookup #-}
  {-# INLINE update #-}
  {-# INLINE write #-}
  {-# INLINE setStable #-}

instance (Identifiable k, IsEmpty (cache a b), ArrowApply c, Profunctor c, ArrowReuse a b (CacheT cache a b c)) => ArrowReuse (k,a) b (CacheT (Group cache) (k,a) b c) where
  reuse s f = proc (k,a0) -> withCache (reuse s (\a a' -> f (k,a) (k,a'))) -<< (k,a0)
  {-# INLINE reuse #-}

withCache :: (Identifiable k, IsEmpty (cache a b), Arrow c, Profunctor c) => CacheT cache a b c x y -> CacheT (Group cache) (k,a) b c (k,x) y
withCache f = CacheT $ modify $ proc ((k,x),g) -> do
  let Groups groups = g
  (cache',y) <- liftCacheT' f -< (fromMaybe empty (M.lookup k groups),x)
  returnA -< (y,Groups (M.insert k cache' groups))
{-# INLINE withCache #-}

instance Identifiable k => IsList (Group cache (k,a) b) where
  type Item (Group cache (k,a) b) = (k,cache a b)
  toList (Groups m) = M.toList m
  fromList l = Groups $ M.fromList l

instance (Show k, Show (cache a b)) => Show (Group cache (k,a) b) where
  show (Groups m) = show (M.toList m)

------ Monotone Cache ------
data Monotone a b where
  Monotone :: s -> Monotone s s

instance IsEmpty s => IsEmpty (Monotone s s) where
  empty = Monotone empty

instance Show s => Show (Monotone s s) where
  show (Monotone s) = show s

instance (ArrowChoice c, Profunctor c) => ArrowCache s s (CacheT Monotone s s c) where
  type Widening (CacheT Monotone s s c) = W.Widening s
  initialize = id
  lookup = CacheT $ proc s -> returnA -< Just (Unstable, s)
  update = CacheT $ askConst $ \widening -> modify' $ \((_,sNew), Monotone sOld) ->
    let y@(_,sWiden) = widening sOld sNew
    in (y,Monotone sWiden)
  write = CacheT $ proc _ -> returnA -< ()
  setStable = CacheT $ proc _ -> returnA -< ()
  {-# INLINE initialize #-}
  {-# INLINE lookup #-}
  {-# INLINE write #-}
  {-# INLINE update #-}
  {-# INLINE setStable #-}


------ Product Cache ------
data (**) cache1 cache2 a b where
  Product :: cache1 a1 b1 -> cache2 a2 b2 -> (**) cache1 cache2 (a1,a2) (b1,b2)

instance (IsEmpty (cache1 a1 b1), IsEmpty (cache2 a2 b2)) => IsEmpty ((**) cache1 cache2 (a1,a2) (b1,b2)) where
  empty = Product empty empty

instance (Show (cache1 a1 b1), Show (cache2 a2 b2)) => Show ((**) cache1 cache2 (a1,a2) (b1,b2)) where
  show (Product c1 c2) = show (c1,c2)

instance (Arrow c, Profunctor c, ArrowCache a1 b1 (CacheT cache1 a1 b1 c), ArrowCache a2 b2 (CacheT cache2 a2 b2 c))
  => ArrowCache (a1,a2) (b1,b2) (CacheT (cache1 ** cache2) (a1,a2) (b1,b2) c) where
  type Widening (CacheT (cache1 ** cache2) (a1,a2) (b1,b2) c) =
    (Cache.Widening (CacheT cache1 a1 b1 c), Cache.Widening (CacheT cache2 a2 b2 c))
  initialize = initialize ** initialize
  lookup = rmap lubMaybe (lookup ** lookup)
  update = dimap (\((a1,a2),(b1,b2)) -> ((a1,b1),(a2,b2))) lubStable (update ** update)
  write = dimap (\((a1,a2),(b1,b2),s) -> ((a1,b1,s),(a2,b2,s))) (const ()) (write ** write)
  setStable = dimap (\(s,(a1,a2)) -> ((s,a1),(s,a2))) (const ()) (setStable ** setStable)
  {-# INLINE initialize #-}
  {-# INLINE lookup #-}
  {-# INLINE write #-}
  {-# INLINE update #-}
  {-# INLINE setStable #-}

(**) :: Arrow c => CacheT cache1 a1 b1 c x1 y1 -> CacheT cache2 a2 b2 c x2 y2 -> CacheT (cache1 ** cache2) (a1,a2) (b1,b2) c (x1,x2) (y1,y2)
(**) f g = CacheT $ lift $ \(w1,w2) -> lift $ proc (c,(x1,x2)) -> do
  let Product c1 c2 = c
  (c1',y1) <- unlift f w1 -< (c1,x1)
  (c2',y2) <- unlift g w2 -< (c2,x2)
  returnA -< (Product c1' c2',(y1,y2))
{-# INLINE (**) #-}

lubMaybe :: (Maybe (Stable,b1), Maybe (Stable,b2)) -> Maybe (Stable,(b1,b2))
lubMaybe (Just x, Just y) = Just (lubStable (x,y))
lubMaybe _ = Nothing

lubStable :: ((Stable,b1),(Stable,b2)) -> (Stable,(b1,b2))
lubStable ((s1,b1),(s2,b2)) = (s1 ⊔ s2, (b1,b2))
{-# INLINE lubStable #-}


------ Context Cache ------
data Context ctx cache a b = Context (HashMap ctx a) (cache a b)

instance IsEmpty (cache a b) => IsEmpty (Context ctx cache a b) where
  empty = Context M.empty empty

instance (Show ctx, Show a, Show (cache a b)) => Show (Context ctx cache a b) where
  show (Context m cache) = show (M.toList m, cache)

instance (Arrow c, Profunctor c, ArrowCache a b (CacheT cache a b c)) => ArrowCache a b (CacheT (Context ctx cache) a b c) where
  type Widening (CacheT (Context ctx cache) a b c) = (W.Widening a, Cache.Widening (CacheT cache a b c))
  initialize = withCache' Cache.initialize
  lookup = withCache' Cache.lookup
  update = withCache' update
  write = withCache' Cache.write
  setStable = withCache' Cache.setStable
  {-# INLINE initialize #-}
  {-# INLINE lookup #-}
  {-# INLINE update #-}
  {-# INLINE write #-}
  {-# INLINE setStable #-}

instance (Identifiable ctx, PreOrd a, Profunctor c, ArrowChoice c, ArrowContext ctx c) => ArrowJoinContext a (CacheT (Context ctx cache) a b c) where
  joinByContext = CacheT $ askConst $ \(widen,_) -> proc a -> do
    ctx <- Context.askContext -< ()
    Context ctxCache cache <- get -< ()
    case M.lookup ctx ctxCache of
      -- If there exists a stable cached entry and the actual input is
      -- smaller than the cached input, recurse the cached input.
      Just a'
        | a ⊑ a' -> returnA -< a'
        | otherwise -> do
          -- If there exists the actual input is not smaller than the cached
          -- input, widen the input.
          let (_,a'') = widen a' a
          put -< Context (M.insert ctx a'' ctxCache) cache
          returnA -< a''
      Nothing -> do
        put -< Context (M.insert ctx a ctxCache) cache
        returnA -< a
  {-# INLINE joinByContext #-}

withCache' :: (Arrow c) => CacheT cache a b c x y -> CacheT (Context ctx cache) a b c x y
withCache' f = lift $ \(_,w2) -> proc (Context ctx cache, x) -> do
  (cache',y) <- unlift f w2 -< (cache,x)
  returnA -< (Context ctx cache',y)
{-# INLINE withCache' #-}
