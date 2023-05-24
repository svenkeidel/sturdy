{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable where

import           Prelude hiding (pred,lookup,map,head,iterate,(.),id,truncate,elem,product,(**))

import           Control.Category
import           Control.Arrow hiding ((<+>))
import           Control.Arrow.Primitive
import           Control.Arrow.Strict
import           Control.Arrow.Trans
import           Control.Arrow.State
import           Control.Arrow.Fix.Reuse(ArrowReuse(..))
import           Control.Arrow.Fix.CallCount as CallCount
import           Control.Arrow.Fix.ControlFlow as ControlFlow
import           Control.Arrow.Fix.Context as Context
import           Control.Arrow.Fix.Cache as Cache
import           Control.Arrow.Order (ArrowLowerBounded)
import qualified Control.Arrow.Order as Order
import           Control.Arrow.Transformer.State

import           Data.Profunctor.Unsafe
import           Data.Empty
import           Data.Order hiding (lub)
import           Data.Coerce
import           Data.Identifiable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.Monoidal
import           Data.Maybe
import           Prettyprinter

import           Data.Abstract.Stable
import qualified Data.Abstract.Widening as W

import           GHC.Exts

newtype CacheT cache a b c x y = CacheT { unCacheT :: StateT (cache a b) c x y}
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowStrict,ArrowTrans,
            ArrowState (cache a b),ArrowControlFlow stmt, ArrowPrimitive, ArrowCFG graph,
            ArrowCallCount a', ArrowCallSite ctx, ArrowContext ctx a')

instance (IsEmpty (cache a b), ArrowRun c) => ArrowRun (CacheT cache a b c) where
  type Run (CacheT cache a b c) x y = Run c x (cache a b,y)
  run f = run (lmap (\x -> (empty, x)) (unlift f))
  {-# INLINE run #-}

instance ArrowLift (CacheT cache a b c) where
  type Underlying (CacheT cache a b c) x y = c (cache a b, x) (cache a b, y)

instance (Arrow c, Profunctor c) => ArrowGetCache (cache a b) (CacheT cache a b c) where
  getCache = CacheT get

instance (Profunctor c,ArrowApply c) => ArrowApply (CacheT cache a b c) where
  app = CacheT (app .# first coerce)
  {-# INLINE app #-}

----- Basic Cache -----
newtype Cache a b = Cache { getMap :: HashMap a (Stable,b)}

instance IsEmpty (Cache a b) where
  empty = Cache M.empty

instance (Show a, Show b) => Show (Cache a b) where
  show (Cache m) = show (M.toList m)

instance (Pretty a, Pretty b) => Pretty (Cache a b) where
  pretty (Cache m) = list [ pretty k <+> prettyStable s <+> pretty v | (k,(s,v)) <- M.toList m]
    where
      prettyStable Stable = "->"
      prettyStable Unstable = "~>"

instance (Identifiable a, LowerBounded b, ArrowChoice c, Profunctor c)
  => ArrowCache a b (CacheT Cache a b c) where
  type Widening (CacheT Cache a b c) = W.Widening b

  initialize = CacheT $ modify' $ \(a,Cache cache) ->
    let cache' = M.insertWith (\_ _old -> _old) a (Unstable,bottom) cache
        ~(_,b) = M.lookupDefault (Unstable,bottom) a cache
    in (b,Cache cache')
  lookup = CacheT $ proc a -> do
    Cache cache <- get -< ()
    returnA -< M.lookup a cache
  update = CacheT $ proc (st,a,b) -> do
    Cache cache <- get -< ()
    case M.lookup a cache of
      Just (_,b') -> do
        let (st',b'') = ?cacheWidening b' b
        put -< Cache (M.insert a (st ⊔ st',b'') cache)
        returnA -< (st',a,b'')
      Nothing -> do
        put -< Cache (M.insert a (Unstable,b) cache)
        returnA -< (Unstable,a,b)
  write = CacheT $ modify' (\((a,b,s),Cache cache) -> ((),Cache (M.insert a (s,b) cache)))
  setStable = CacheT $ modify' $ \((s,a),Cache cache) -> ((),Cache (M.adjust (first (const s)) a cache))
  {-# INLINE initialize #-}
  {-# INLINE lookup #-}
  {-# INLINE write #-}
  {-# INLINE update #-}
  {-# INLINE setStable #-}
  {-# SCC initialize #-}
  {-# SCC lookup #-}
  {-# SCC write #-}
  {-# SCC update #-}
  {-# SCC setStable #-}

instance (Arrow c, Profunctor c) => ArrowIterateCache a b (CacheT Cache a b c) where
  nextIteration = CacheT $ proc x -> do
    put -< empty
    returnA -< x
  {-# INLINE nextIteration #-}

instance (PreOrd a, Arrow c, Profunctor c) => ArrowReuse a b (CacheT Cache a b c) where
  reuse f = CacheT $ proc (a,s) -> do
    Cache cache <- get -< ()
    returnA -< M.foldlWithKey' (\m a' (s',b') -> if s' ⊑ s && a ⊑ a' then m <> f a a' s' b' else m) mempty cache
  {-# INLINE reuse #-}

instance (LowerBounded b, Profunctor c, Arrow c) => ArrowLowerBounded b (CacheT Cache a b c) where
  bottom = proc _ -> returnA -< bottom
  {-# INLINE bottom #-}

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
  initialize = withGroup Cache.initialize
  lookup = withGroup Cache.lookup
  update = proc (st,(k,a),b) -> do
    (st',a',b') <- withGroup Cache.update -< (k,(st,a,b))
    returnA -< (st',(k,a'),b')
  write = lmap (\((k,a),b,s) -> (k,(a,b,s))) (withGroup Cache.write)
  setStable = lmap shuffle1 (withGroup Cache.setStable)
  {-# INLINE initialize #-}
  {-# INLINE lookup #-}
  {-# INLINE update #-}
  {-# INLINE write #-}
  {-# INLINE setStable #-}
  {-# SCC initialize #-}
  {-# SCC lookup #-}
  {-# SCC write #-}
  {-# SCC update #-}
  {-# SCC setStable #-}

withGroup :: (Identifiable k, IsEmpty (cache a b),
              Profunctor c, Arrow c)
          => CacheT cache a b c x y -> CacheT (Group cache) (k,a) b c (k,x) y
withGroup f = lift $
  dimap (\(Groups groups,(k,x)) -> ((groups,k),(fromMaybe empty (M.lookup k groups),x)))
        (\((groups,k),(cache,y)) -> (Groups (M.insert k cache groups), y))
        (second (unlift f))
{-# INLINE withGroup #-}


instance (Identifiable k, IsEmpty (cache a b), ArrowApply c, Profunctor c, ArrowReuse a b (CacheT cache a b c)) => ArrowReuse (k,a) b (CacheT (Group cache) (k,a) b c) where
  reuse f = proc ((k,a0),s) -> withGroup (reuse (\a a' -> f (k,a) (k,a'))) -<< (k,(a0,s))
  {-# INLINE reuse #-}

instance Identifiable k => IsList (Group cache (k,a) b) where
  type Item (Group cache (k,a) b) = (k,cache a b)
  toList (Groups m) = M.toList m
  fromList l = Groups $ M.fromList l

instance (Show k, Show (cache a b)) => Show (Group cache (k,a) b) where
  show (Groups m) = show (M.toList m)

------ Parallel Cache ------
data Parallel cache a b =
  Parallel { old :: cache a b
           , new :: cache a b
           , stable :: !Stable
           }

instance Pretty (cache a b) => Show (Parallel cache a b)   where show = show . pretty
instance Pretty (cache a b) => Pretty (Parallel cache a b) where
  pretty (Parallel o n s) =
    vsep ["Parallel",
          "Old:" <+> align (pretty o),
          "New:" <+> align (pretty n),
          "Stable:" <> viaShow s
         ]

instance IsEmpty (cache a b) => IsEmpty (Parallel cache a b) where
  empty = Parallel { old = empty, new = empty, stable = Stable }

instance (Profunctor c, Arrow c,
          ArrowLowerBounded b (CacheT cache a b c))
    => ArrowLowerBounded b (CacheT (Parallel cache) a b c) where
  bottom = newCache Order.bottom
  {-# INLINE bottom #-}

instance (Profunctor c, ArrowChoice c,
          ArrowIterateCache a b (CacheT cache a b c),
          ArrowCache a b (CacheT cache a b c))
    => ArrowParallelCache a b (CacheT (Parallel cache) a b c) where
  lookupOldCache = oldCache $ proc a -> do
    m <- lookup -< a; case m of
      Just (_,b) -> returnA -< b
      Nothing    -> initialize -< a
  lookupNewCache = newCache (rmap (fmap snd) lookup)
  updateNewCache = dimap (\(a,b) -> (Stable,a,b)) (\(_,_,b) -> b) update
  isStable = modify' (\(_,p) -> (stable p,p))
  {-# INLINE lookupOldCache #-}
  {-# INLINE lookupNewCache #-}
  {-# INLINE updateNewCache #-}
  {-# INLINE isStable #-}
  {-# SCC lookupOldCache #-}
  {-# SCC lookupNewCache #-}
  {-# SCC updateNewCache #-}
  {-# SCC isStable #-}

instance (Profunctor c, ArrowChoice c,
          ArrowIterateCache a b (CacheT cache a b c))
    => ArrowIterateCache a b (CacheT (Parallel cache) a b c) where
  nextIteration = proc x -> do
    modify' (\(_,p) -> ((),p { old = new p, stable = Stable })) -< ()
    newCache nextIteration -< x
  {-# INLINE nextIteration #-}

instance (Profunctor c, ArrowChoice c,
          ArrowCache a b (CacheT cache a b c))
    => ArrowCache a b (CacheT (Parallel cache) a b c) where
  type Widening (CacheT (Parallel cache) a b c) = Cache.Widening (CacheT cache a b c)
  initialize = proc a -> do
    m <- oldCache lookup -< a; case m of
      Just (s,b) -> do
        newCache write -< (a,b,s)
        returnA -< b
      Nothing -> do
        modify' (\((),cache) -> ((),cache { stable = Unstable })) -< ()
        newCache initialize -< a
  lookup = newCache lookup
  update = proc (st,a,b) -> do
    (st',a',b') <- newCache update -< (st,a,b)
    st'' <- modify' (\(st',cache) -> let st'' = stable cache ⊔ st' in (st'',cache { stable = st'' })) -< st'
    returnA -< (st'',a',b')
  write = newCache write
  setStable = newCache setStable
  {-# INLINE initialize #-}
  {-# INLINE lookup #-}
  {-# INLINE write #-}
  {-# INLINE update #-}
  {-# INLINE setStable #-}

-- Note: All operations on the old cache are read-only.
oldCache :: (Arrow c, Profunctor c) => CacheT cache a b c x y -> CacheT (Parallel cache) a b c x y
oldCache f = lift $ dimap (\(p,x) -> (p,(old p,x))) (\(p,(_,y)) -> (p, y)) (second (unlift f))
{-# INLINE oldCache #-}

newCache :: (Arrow c, Profunctor c) => CacheT cache a b c x y -> CacheT (Parallel cache) a b c x y
newCache f = lift $
  dimap (\(p,x) -> (p,(new p,x)))
        (\(p,(n,y)) -> (p { new = n }, y))
        (second (unlift f))
{-# INLINE newCache #-}

------ Monotone Cache ------
data Monotone a b where
  Monotone :: s -> HashMap a (Stable,s,b) -> Monotone (s,a) (s,b)

instance IsEmpty s => IsEmpty (Monotone (s,a) (s,b)) where
  empty = Monotone empty empty

instance (Show s, Show a, Show b) => Show (Monotone (s,a) (s,b)) where
  show (Monotone s m) = show (s,m)

instance (Pretty s, Pretty a, Pretty b) => Pretty (Monotone (s,a) (s,b)) where
  pretty (Monotone _ m) =
    align (list [ pretty s <+> "|" <+> pretty a <+> showArrow st <+> pretty b | (a,(st,s,b)) <- M.toList m])

instance (Identifiable a, PreOrd s, LowerBounded b, ArrowChoice c, Profunctor c)
    => ArrowCache (s,a) (s,b) (CacheT Monotone (s,a) (s,b) c) where
  type Widening (CacheT Monotone (s,a) (s,b) c) = (W.Widening s,W.Widening b)
  initialize = CacheT $ modify' $ \((sNew,a),Monotone s cache) ->
    case M.lookup a cache of
      Just (_,sOld,b)
        | sNew ⊑ sOld -> ((sNew,b),      Monotone s cache)
        | otherwise   -> ((sNew,b),      Monotone s (M.insert a (Unstable,sNew,b) cache))
      Nothing         -> ((sNew,bottom), Monotone s (M.insert a (Unstable,sNew,bottom) cache))
  lookup = CacheT $ modify' $ \((s,a),m@(Monotone _ cache)) ->
    case M.lookup a cache of
      Just (st,s',b) | s ⊑ s' -> (Just (st,(s,b)), m)
      _ -> (Nothing, m)
  update = CacheT $ modify' $ \((stable0,(sNew,a),(sNew',bNew)),Monotone s cache) ->
    let (widenS,widenB) = ?cacheWidening
        (_,sWiden) = widenS s sNew'
    in case M.lookup a cache of
      Just (_,sOld,bOld) ->
          let stable1 = if sNew ⊑ sOld then Stable else Unstable
              (stable2,bWiden) = widenB bOld bNew
          in ((stable1 ⊔ stable2, (sWiden,a), (sWiden,bWiden)),
              Monotone sWiden (M.insert a (stable0 ⊔ stable1 ⊔ stable2,sNew,bWiden) cache))
      Nothing -> ((Unstable,(sWiden,a),(sWiden,bNew)), Monotone sWiden (M.insert a (Unstable,sNew,bNew) cache))
  write = CacheT $ modify' $ \(((sNew, a), (_,b), st), Monotone s cache) ->
    ((), Monotone s (M.insert a (st, sNew, b) cache))
  setStable = CacheT $ proc _ -> returnA -< ()
  {-# INLINE initialize #-}
  {-# INLINE lookup #-}
  {-# INLINE write #-}
  {-# INLINE update #-}
  {-# INLINE setStable #-}
  {-# SCC initialize #-}
  {-# SCC lookup #-}
  {-# SCC write #-}
  {-# SCC update #-}
  {-# SCC setStable #-}

instance (Arrow c, Profunctor c) => ArrowIterateCache (s,a) (s,b) (CacheT Monotone (s,a) (s,b) c) where
  nextIteration = CacheT $ proc ((_,a),(sNew,b)) -> do
    put -< Monotone sNew empty
    returnA -< ((sNew,a),(sNew,b))
  {-# INLINE nextIteration #-}

-- | Cache for an analysis with abstract garbage collection. The type variable
-- @s@ stands for the store.
data GarbageCollect a b where
  GarbageCollect :: HashMap a (Stable,s,s,b) -> GarbageCollect (s,a) (s,b)

instance IsEmpty (GarbageCollect (s,a) (s,b)) where
  empty = GarbageCollect empty

instance (Show s, Show a, Show b) => Show (GarbageCollect (s,a) (s,b)) where
  show (GarbageCollect m) = show m

instance (Pretty s, Pretty a, Pretty b) => Pretty (GarbageCollect (s,a) (s,b)) where
  pretty (GarbageCollect m) =
    align (list [ pretty s1 <+> "×" <+> pretty a <+> showArrow st <+> pretty s2 <+> "×" <+> pretty b | (a,(st,s1,s2,b)) <- M.toList m])

instance (Identifiable a, PreOrd s, LowerBounded b, ArrowChoice c, Profunctor c)
    => ArrowCache (s,a) (s,b) (CacheT GarbageCollect (s,a) (s,b) c) where
  type Widening (CacheT GarbageCollect (s,a) (s,b) c) = (W.Widening s,W.Widening b)
  initialize = CacheT $ modify' $ \((sNew1,a),GarbageCollect cache) ->
    let (widenS,_) = ?cacheWidening
    in case M.lookup a cache of
      Just (_,sOld1,sOld2,b)
        | sNew1 ⊑ sOld1 -> ((sOld2,b), GarbageCollect cache)
        | otherwise     -> let (_,sWiden1) = widenS sOld1 sNew1
                               (_,sWiden2) = widenS sOld2 sNew1
                           in ((sWiden2,b), GarbageCollect (M.insert a (Unstable,sWiden1,sOld2,b) cache))
      Nothing           -> ((sNew1,bottom), GarbageCollect (M.insert a (Unstable,sNew1,sNew1,bottom) cache))
  lookup = CacheT $ modify' $ \((sNew1,a),m@(GarbageCollect cache)) ->
    case M.lookup a cache of
      Just (st,sOld1,sOld2,b)
        | sNew1 ⊑ sOld1 -> (Just (st,(sOld2,b)),       m)
        | otherwise     -> (Just (Unstable,(sOld2,b)), m)
      _ -> (Nothing, m)
  update = CacheT $ modify' $ \((stable0,(sNew1,a),(sNew2,bNew)),GarbageCollect cache) ->
    let (widenS,widenB) = ?cacheWidening
    in case M.lookup a cache of
      Just (_,sOld1,sOld2,bOld) ->
          let (stable1,sWiden1) = widenS sOld1 sNew1
              (stable2,sWiden2) = widenS sOld2 sNew2
              (stable3,bWiden)  = widenB bOld bNew
          in ((stable1 ⊔ stable2 ⊔ stable3, (sWiden1,a), (sWiden2,bWiden)),
              GarbageCollect (M.insert a (stable0 ⊔ stable1 ⊔ stable2 ⊔ stable3, sWiden1, sWiden2, bWiden) cache))
      Nothing -> ((Unstable,(sNew1,a),(sNew2,bNew)), GarbageCollect (M.insert a (Unstable,sNew1,sNew2,bNew) cache))
  write = CacheT $ modify' $ \(((sNew1, a), (sNew2,b), st), GarbageCollect cache) ->
    ((), GarbageCollect (M.insert a (st, sNew1, sNew2, b) cache))
  setStable = CacheT $ proc _ -> returnA -< ()
  {-# INLINE initialize #-}
  {-# INLINE lookup #-}
  {-# INLINE write #-}
  {-# INLINE update #-}
  {-# INLINE setStable #-}
  {-# SCC initialize #-}
  {-# SCC lookup #-}
  {-# SCC write #-}
  {-# SCC update #-}
  {-# SCC setStable #-}

instance (Arrow c, Profunctor c) => ArrowIterateCache (s,a) (s,b) (CacheT GarbageCollect (s,a) (s,b) c) where
  nextIteration = CacheT $ proc ((_,a),(sNew2,b)) -> do
    put -< GarbageCollect empty
    returnA -< ((sNew2,a),(sNew2,b))
  {-# INLINE nextIteration #-}

------ Monotone Cache that factors out the monotone element ------
data MonotoneFactor a b where
  MonotoneFactor :: s -> HashMap a b -> MonotoneFactor (s,a) (s,b)

instance IsEmpty s => IsEmpty (MonotoneFactor (s,a) (s,b)) where
  empty = MonotoneFactor empty empty

instance (Show s, Show a, Show b) => Show (MonotoneFactor (s,a) (s,b)) where
  show (MonotoneFactor s m) = show (s,m)

instance (Pretty s, Pretty a, Pretty b) => Pretty (MonotoneFactor (s,a) (s,b)) where
  pretty (MonotoneFactor s m) =
    vsep [ "Monotone:" <+> pretty s
         , "NonMonotone:" <+> align (list [ pretty k <+> "->" <+> pretty v | (k,v) <- M.toList m])
         ]

instance (Identifiable a, LowerBounded b,
          ArrowChoice c, Profunctor c)
    => ArrowCache (s,a) (s,b) (CacheT MonotoneFactor (s,a) (s,b) c) where
  type Widening (CacheT MonotoneFactor (s,a) (s,b) c) = (W.Widening s,W.Widening b)
  initialize = CacheT $ modify' $ \((s,a),MonotoneFactor s' cache) ->
    let cache' = M.insertWith (\_ _old -> _old) a bottom cache
        b = M.lookupDefault bottom a cache'
    in ((s,b),MonotoneFactor s' cache')
  lookup = CacheT $ modify' $ \((s,a),m@(MonotoneFactor _ cache)) ->
    ((\b -> (Unstable,(s,b))) <$> M.lookup a cache, m)
  update = CacheT $ modify' $ \((_,(_,a),(sNew,b)),MonotoneFactor sOld cache) ->
    let (widenS,widenB) = ?cacheWidening
        (stable1,sWiden) = widenS sOld sNew
    in case M.lookup a cache of
        Just b' ->
          let (stable2,b'') = widenB b' b
          in ((stable1 ⊔ stable2, (sWiden,a), (sWiden,b'')),
              MonotoneFactor sWiden (M.insert a b'' cache))
        Nothing -> ((Unstable,(sWiden,a), (sWiden,b)),MonotoneFactor sWiden (M.insert a b cache))
  write = CacheT $ modify' $ \(((_, a), (_, b), _),MonotoneFactor s cache) -> ((),MonotoneFactor s (M.insert a b cache))
  setStable = CacheT $ proc _ -> returnA -< ()
  {-# INLINE initialize #-}
  {-# INLINE lookup #-}
  {-# INLINE write #-}
  {-# INLINE update #-}
  {-# INLINE setStable #-}
  {-# SCC initialize #-}
  {-# SCC lookup #-}
  {-# SCC write #-}
  {-# SCC update #-}
  {-# SCC setStable #-}

instance (Arrow c, Profunctor c) => ArrowIterateCache (s,a) (s,b) (CacheT MonotoneFactor (s,a) (s,b) c) where
  nextIteration = CacheT $ proc ((_,a),(sNew,b)) -> do
    put -< MonotoneFactor sNew empty
    returnA -< ((sNew,a),(sNew,b))
  {-# INLINE nextIteration #-}
