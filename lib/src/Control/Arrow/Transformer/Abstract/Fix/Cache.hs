{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Arrow.Transformer.Abstract.Fix.Cache where

import Prelude hiding (pred,lookup,map,head,iterate,(.),id,truncate,elem,product,(**))

import Control.Category
import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Trans
import Control.Arrow.State
import Control.Arrow.Fix.Context as Context
import Control.Arrow.Fix.Cache as Cache
import Control.Arrow.Fix.Parallel as Parallel
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

type family Widening c :: *

newtype CacheT cache a b c x y = CacheT { unCacheT :: ConstT (Widening (cache a b)) (StateT (cache a b) c) x y}
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowState (cache a b))

instance ArrowTrans (CacheT cache a b c) where
  type Underlying (CacheT cache a b c) x y = Widening (cache a b) -> c (cache a b, x) (cache a b, y)

runCacheT :: (IsEmpty (cache a b), Profunctor c) => Widening (cache a b) -> CacheT cache a b c x y -> c x (cache a b,y)
runCacheT widen (CacheT f) = lmap (empty,) (runStateT (runConstT widen f))
{-# INLINE runCacheT #-}

instance (IsEmpty (cache a b), ArrowRun c) => ArrowRun (CacheT cache a b c) where
  type Run (CacheT cache a b c) x y = Widening (cache a b) -> Run c x (cache a b,y)
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

type instance Widening (Cache a b) = W.Widening b

instance IsEmpty (Cache a b) where
  empty = Cache M.empty

instance (Show a, Show b) => Show (Cache a b) where
  show (Cache m) = show (M.toList m)

instance (Identifiable a, LowerBounded b, ArrowChoice c, Profunctor c)
  => ArrowCache a b (CacheT Cache a b c) where
  initialize = CacheT $ modify' $ \(a,Cache cache) ->
    let cache' = M.insertWith (\_ _old -> _old) a (Unstable,bottom) cache
        ~(_,b) = M.lookupDefault (Unstable,bottom) a cache
    in (b,Cache cache')
  lookup = CacheT $ proc a -> do
    Cache cache <- get -< ()
    returnA -< M.lookup a cache
  update = CacheT $ askConst $ \widen -> proc (a,b) -> do
    Cache cache <- get -< ()
    case M.lookup a cache of
      Just (Stable,b') ->
        returnA -< (Stable,b')
      Just (Unstable,b') -> do
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

instance Identifiable a => IsList (Cache a b) where
  type Item (Cache a b) = (a,b,Stable)
  toList (Cache m) = [ (a,b,s) | (a,(s,b)) <- M.toList m]
  fromList l = Cache $ M.fromList [ (a,(s,b)) | (a,b,s) <- l]

------ Group Cache ------
data Group cache a b where
  Groups :: HashMap k (cache a b) -> Group cache (k,a) b

type instance Widening (Group cache (k,a) b) = Widening (cache a b)

instance IsEmpty (Group cache (k,a) b) where
  empty = Groups empty
  {-# INLINE empty #-}

instance (Identifiable k, Arrow c, Profunctor c, ArrowCache a b (CacheT cache a b c), IsEmpty (cache a b)) => ArrowCache (k,a) b (CacheT (Group cache) (k,a) b c) where
  initialize = withGroup Cache.initialize
  lookup = withGroup Cache.lookup
  update = lmap assoc2 (withGroup Cache.update)
  write = lmap (\((k,a),b,s) -> (k,(a,b,s))) (withGroup Cache.write)
  setStable = lmap shuffle1 (withGroup Cache.setStable)
  {-# INLINE initialize #-}
  {-# INLINE lookup #-}
  {-# INLINE update #-}
  {-# INLINE write #-}
  {-# INLINE setStable #-}

instance (Identifiable k, IsEmpty (cache a b), ArrowApply c, Profunctor c, ArrowJoinContext a (CacheT cache a b c)) => ArrowJoinContext (k,a) (CacheT (Group cache) (k,a) b c) where
  joinByContext = proc (k,a) -> do
    a' <- withGroup joinByContext -< (k,a)
    returnA -< (k,a')
  {-# INLINE joinByContext #-}

withGroup :: (Identifiable k, IsEmpty (cache a b), Arrow c, Profunctor c) => CacheT cache a b c x y -> CacheT (Group cache) (k,a) b c (k,x) y
withGroup f = lift $ \widen ->
  dimap (\(Groups groups,(k,x)) -> ((groups,k),(fromMaybe empty (M.lookup k groups),x)))
        (\((groups,k),(cache,y)) -> (Groups (M.insert k cache groups), y))
        (second (unlift f widen))
{-# INLINE withGroup #-}

instance Identifiable k => IsList (Group cache (k,a) b) where
  type Item (Group cache (k,a) b) = (k,cache a b)
  toList (Groups m) = M.toList m
  fromList l = Groups $ M.fromList l

instance (Show k, Show (cache a b)) => Show (Group cache (k,a) b) where
  show (Groups m) = show (M.toList m)

------ Parallel Cache ------
data Parallel cache a b = Parallel { old :: cache a b, new :: cache a b, stable :: !Stable }
  deriving (Show)

type instance Widening (Parallel cache a b) = Widening (cache a b)

instance IsEmpty (cache a b) => IsEmpty (Parallel cache a b) where
  empty = Parallel { old = empty, new = empty, stable = Stable }

instance (IsEmpty (cache a b), Arrow c, Profunctor c) => ArrowParallel (CacheT (Parallel cache) a b c) where
  nextIteration = modify' (\((),p) -> ((),Parallel { old = new p, new = empty, stable = Stable }))
  {-# INLINE nextIteration #-}

instance (ArrowChoice c, Profunctor c, ArrowCache a b (CacheT cache a b c))
  => ArrowCache a b (CacheT (Parallel cache) a b c) where
  initialize = proc a -> do
    m <- oldCache lookup -< a
    case m of
      Just (s,b) -> do
        newCache write -< (a,b,s)
        returnA -< b
      Nothing -> do
        modify' (\((),cache) -> ((),cache { stable = Unstable })) -< ()
        newCache initialize -< a
  lookup = newCache lookup
  update = proc (a,b) -> do
    (s,b') <- newCache update -< (a,b)
    s' <- modify' (\(s,cache) -> let s' = stable cache ⊔ s in (s',cache { stable = s' })) -< s
    returnA -< (s',b')
  write = newCache write
  setStable = newCache setStable
  {-# INLINE initialize #-}
  {-# INLINE lookup #-}
  {-# INLINE write #-}
  {-# INLINE update #-}
  {-# INLINE setStable #-}

oldCache :: (Arrow c, Profunctor c) => CacheT cache a b c x y -> CacheT (Parallel cache) a b c x y
oldCache f = lift $ \widen ->
  dimap (\(p,x) -> ((new p,stable p),(old p,x)))
        (\((n,s),(o,y)) -> (Parallel { old = o, new = n, stable = s}, y))
        (second (unlift f widen))
{-# INLINE oldCache #-}

newCache :: (Arrow c, Profunctor c) => CacheT cache a b c x y -> CacheT (Parallel cache) a b c x y
newCache f = lift $ \widen ->
  dimap (\(p,x) -> ((old p,stable p),(new p,x)))
        (\((o,s),(n,y)) -> (Parallel { old = o, new = n, stable = s}, y))
        (second (unlift f widen))
{-# INLINE newCache #-}

------ Monotone Cache ------
data Monotone a b where
  Monotone :: s -> Monotone s s

type instance Widening (Monotone s s) = W.Widening s

instance IsEmpty s => IsEmpty (Monotone s s) where
  empty = Monotone empty

instance Show s => Show (Monotone s s) where
  show (Monotone s) = show s

instance (Arrow c, Profunctor c) => ArrowParallel (CacheT Monotone a b c) where
  nextIteration = id
  {-# INLINE nextIteration #-}

instance (ArrowChoice c, Profunctor c) => ArrowCache s s (CacheT Monotone s s c) where
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

type instance Widening ((cache1 ** cache2) (a1,a2) (b1,b2)) = (Widening (cache1 a1 b1), Widening (cache2 a2 b2))

instance (Arrow c, Profunctor c, ArrowParallel (CacheT cache1 a1 b1 c), ArrowParallel (CacheT cache2 a2 b2 c)) => ArrowParallel (CacheT (cache1 ** cache2) (a1,a2) (b1,b2) c) where
  nextIteration = proc () -> do
    nextIteration ** nextIteration -< ((),())
    returnA -< ()
  {-# INLINE nextIteration #-}

instance (IsEmpty (cache1 a1 b1), IsEmpty (cache2 a2 b2)) => IsEmpty ((**) cache1 cache2 (a1,a2) (b1,b2)) where
  empty = Product empty empty

instance (Show (cache1 a1 b1), Show (cache2 a2 b2)) => Show ((**) cache1 cache2 (a1,a2) (b1,b2)) where
  show (Product c1 c2) = show (c1,c2)

instance (Arrow c, Profunctor c, ArrowCache a1 b1 (CacheT cache1 a1 b1 c), ArrowCache a2 b2 (CacheT cache2 a2 b2 c))
  => ArrowCache (a1,a2) (b1,b2) (CacheT (cache1 ** cache2) (a1,a2) (b1,b2) c) where
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

(**) :: (Profunctor c, Arrow c) => CacheT cache1 a1 b1 c x1 y1 -> CacheT cache2 a2 b2 c x2 y2 -> CacheT (cache1 ** cache2) (a1,a2) (b1,b2) c (x1,x2) (y1,y2)
(**) f g = lift $ \(w1,w2) -> dimap (\(Product cache1 cache2,(x1,x2)) -> ((cache1,x1),(cache2,x2))) (\((cache1,x1),(cache2,x2)) -> (Product cache1 cache2,(x1,x2))) (unlift f w1 *** unlift g w2)
{-# INLINE (**) #-}

lubMaybe :: (Maybe (Stable,b1), Maybe (Stable,b2)) -> Maybe (Stable,(b1,b2))
lubMaybe (Just x, Just y) = Just (lubStable (x,y))
lubMaybe _ = Nothing

lubStable :: ((Stable,b1),(Stable,b2)) -> (Stable,(b1,b2))
lubStable ((s1,b1),(s2,b2)) = (s1 ⊔ s2, (b1,b2))
{-# INLINE lubStable #-}

------ Second Projection ------
data Proj2 cache a b where
  Proj2 :: cache a b -> Proj2 cache (u,a) b

type instance Widening (Proj2 cache (u,a) b) = Widening (cache a b)

instance IsEmpty (cache a b) => IsEmpty (Proj2 cache (u,a) b) where
  empty = Proj2 empty
  {-# INLINE empty #-}

-- NOTE: A cache which ignores one of its inputs is possibly unsound.
-- instance (Arrow c, Profunctor c, ArrowCache a b (CacheT cache a b c)) => ArrowCache (u,a) b (CacheT (Proj2 cache) (u,a) b c) where
--   initialize = p2 initialize
--   lookup = p2 lookup
--   update = lmap assoc2 (p2 update)
--   write = lmap (\((u,a),b,s) -> (u,(a,b,s))) (p2 write)
--   setStable = lmap (\(s,(u,a)) -> (u,(s,a))) (p2 setStable)
--   {-# INLINE initialize #-}
--   {-# INLINE lookup #-}
--   {-# INLINE update #-}
--   {-# INLINE write #-}
--   {-# INLINE setStable #-}

-- p2 :: Profunctor c => CacheT cache a b c x2 y -> CacheT (Proj2 cache) (u,a) b c (x1,x2) y
-- p2 f = lift $ \widen -> dimap (\(Proj2 cache,(_,a)) -> (cache,a)) (first Proj2) (unlift f widen)
-- {-# INLINE p2 #-}

instance (Arrow c, Profunctor c, ArrowJoinContext a (CacheT cache a b c)) => ArrowJoinContext (u,a) (CacheT (Proj2 cache) (u,a) b c) where
  joinByContext = lift $ \widen ->
    dimap (\(Proj2 cache,(u,a)) -> (u,(cache,a))) (\(u,(cache,a)) -> (Proj2 cache,(u,a)))
          (second (unlift (joinByContext :: CacheT cache a b c a a) widen))
  {-# INLINE joinByContext #-}


------ Context ------
data Context ctx cache a b = Context (ctx a b) (cache a b)

type instance Widening (Context ctx cache a b) = (Widening (ctx a b), Widening (cache a b))

instance (IsEmpty (ctx a b), IsEmpty (cache a b)) => IsEmpty (Context ctx cache a b) where
  empty = Context empty empty
  {-# INLINE empty #-}

instance (Arrow c, Profunctor c, ArrowCache a b (CacheT cache a b c)) => ArrowCache a b (CacheT (Context ctx cache) a b c) where
  initialize = withCache initialize
  lookup = withCache lookup
  update = withCache update
  write = withCache write
  setStable = withCache setStable
  {-# INLINE initialize #-}
  {-# INLINE lookup #-}
  {-# INLINE update #-}
  {-# INLINE write #-}
  {-# INLINE setStable #-}

instance (Arrow c, Profunctor c, ArrowJoinContext a (CacheT ctx a b c)) => ArrowJoinContext a (CacheT (Context ctx cache) a b c) where
  joinByContext = withCtx joinByContext
  {-# INLINE joinByContext #-}

withCache :: (Profunctor c, Arrow c) => CacheT cache a b c x y -> CacheT (Context ctx cache) a b c x y
withCache f = lift $ \(_,w2) -> dimap (\(Context ctx cache,x) -> (ctx,(cache,x))) (\(ctx,(cache,x2)) -> (Context ctx cache,x2)) (second (unlift f w2))
{-# INLINE withCache #-}

withCtx :: (Profunctor c, Arrow c) => CacheT ctx a b c x y -> CacheT (Context ctx cache) a b c x y
withCtx f = lift $ \(w1,_) -> dimap (\(Context ctx cache, a) -> (cache,(ctx,a))) (\(cache,(ctx,a)) -> (Context ctx cache,a)) (second (unlift f w1))
{-# INLINE withCtx #-}

------ Context Cache ------
newtype CtxCache ctx a b = CtxCache (HashMap ctx a)

type instance Widening (CtxCache ctx a b) = W.Widening a

instance IsEmpty (CtxCache ctx a b) where
  empty = CtxCache empty

instance (Show ctx, Show a) => Show (CtxCache ctx a b) where
  show (CtxCache m) = show (M.toList m)

instance (Identifiable ctx, PreOrd a, Profunctor c, ArrowChoice c, ArrowContext ctx c) => ArrowJoinContext a (CacheT (CtxCache ctx) a b c) where
  joinByContext = lift $ \widen -> proc (CtxCache cache, a) -> do
    ctx <- Context.askContext -< ()
    returnA -< case M.lookup ctx cache of
      -- If there exists a stable cached entry and the actual input is
      -- smaller than the cached input, recurse the cached input.
      Just a'
        | a ⊑ a' -> (CtxCache cache, a')
        | otherwise ->
          -- If there exists the actual input is not smaller than the cached
          -- input, widen the input.
          let (_,a'') = widen a' a
          in (CtxCache (M.insert ctx a'' cache),a'')
      Nothing -> (CtxCache (M.insert ctx a cache),a)
  {-# INLINE joinByContext #-}

