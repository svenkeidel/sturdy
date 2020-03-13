{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable where

import           Prelude hiding (pred,lookup,map,head,iterate,(.),id,truncate,elem,product,(**))

import           Control.Category
import           Control.Arrow hiding ((<+>))
import           Control.Arrow.Strict
import           Control.Arrow.Const
import           Control.Arrow.Trans
import           Control.Arrow.State
import           Control.Arrow.Fix.ControlFlow as ControlFlow
import           Control.Arrow.Fix.Context as Context
import           Control.Arrow.Fix.Cache as Cache
import           Control.Arrow.Order (ArrowJoin(..),ArrowComplete(..),ArrowLowerBounded,ArrowEffectCommutative)
import qualified Control.Arrow.Order as Order

import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Static
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
import           Data.Text.Prettyprint.Doc

import           Data.Abstract.Stable
import qualified Data.Abstract.Widening as W

import           GHC.Exts

type family Widening c :: *

newtype CacheT cache a b c x y = CacheT { unCacheT :: ConstT (Widening (cache a b)) (StateT (cache a b) c) x y}
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowStrict,
            ArrowState (cache a b),ArrowControlFlow stmt)

instance (IsEmpty (cache a b), ArrowRun c) => ArrowRun (CacheT cache a b c) where
  type Run (CacheT cache a b c) x y = Widening (cache a b) -> Run c x (cache a b,y)
  run f widen = {-# SCC "runCacheT" #-} run (lmap (\x -> (empty,x)) (unlift f widen))
  {-# INLINE run #-}

instance ArrowTrans (CacheT cache a b c) where
  type Underlying (CacheT cache a b c) x y = Widening (cache a b) -> c (cache a b, x) (cache a b, y)

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

instance (Pretty a, Pretty b) => Pretty (Cache a b) where
  pretty (Cache m) = list [ pretty k <+> prettyStable s <+> pretty v | (k,(s,v)) <- M.toList m]
    where
      prettyStable Stable = "->"
      prettyStable Unstable = "~>"

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
      Just (_,b') -> do
        let (st,b'') = widen b' b
        put -< Cache (M.insert a (st,b'') cache)
        returnA -< (st,a,b'')
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

instance (Arrow c, Profunctor c) => ArrowIterateCache (CacheT Cache a b c) where
  nextIteration = CacheT $ proc () -> put -< empty
  {-# INLINE nextIteration #-}

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

type instance Widening (Group cache (k,a) b) = Widening (cache a b)

instance IsEmpty (Group cache (k,a) b) where
  empty = Groups empty
  {-# INLINE empty #-}

instance (Identifiable k, Arrow c, Profunctor c, ArrowCache a b (CacheT cache a b c), IsEmpty (cache a b)) => ArrowCache (k,a) b (CacheT (Group cache) (k,a) b c) where
  initialize = withGroup Cache.initialize
  lookup = withGroup Cache.lookup
  update = proc ((k,a),b) -> do
    (st,a',b') <- withGroup Cache.update -< (k,(a,b))
    returnA -< (st,(k,a'),b')
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

instance Pretty (cache a b) => Show (Parallel cache a b)   where show = show . pretty
instance Pretty (cache a b) => Pretty (Parallel cache a b) where
  pretty (Parallel o n s) = vsep ["Parallel", "Old:" <+> align (pretty o), "New:" <+> align (pretty n), "Stable:" <> viaShow s]

type instance Widening (Parallel cache a b) = Widening (cache a b)

instance IsEmpty (cache a b) => IsEmpty (Parallel cache a b) where
  empty = Parallel { old = empty, new = empty, stable = Stable }

instance (Profunctor c, Arrow c, ArrowLowerBounded b (CacheT cache a b c)) => ArrowLowerBounded b (CacheT (Parallel cache) a b c) where
  bottom = newCache Order.bottom
  {-# INLINE bottom #-}

instance (Profunctor c, ArrowChoice c, ArrowIterateCache (CacheT cache a b c), ArrowCache a b (CacheT cache a b c))
    => ArrowParallelCache a b (CacheT (Parallel cache) a b c) where
  lookupOldCache = oldCache (rmap (fmap snd) lookup)
  lookupNewCache = newCache (rmap (fmap snd) lookup)
  updateNewCache = rmap (\(_,_,b) -> b) update
  isStable = modify' (\(_,p) -> (stable p,p))
  {-# INLINE lookupOldCache #-}
  {-# INLINE lookupNewCache #-}
  {-# INLINE updateNewCache #-}
  {-# INLINE isStable #-}

instance (Profunctor c, ArrowChoice c, ArrowIterateCache (CacheT cache a b c)) => ArrowIterateCache (CacheT (Parallel cache) a b c) where
  nextIteration = proc () -> do
    modify' (\(_,p) -> ((),p { old = new p, stable = Stable })) -< ()
    newCache nextIteration -< ()
  {-# INLINE nextIteration #-}

instance (ArrowChoice c, Profunctor c, ArrowCache a b (CacheT cache a b c))
  => ArrowCache a b (CacheT (Parallel cache) a b c) where
  initialize = proc a -> do
    m <- oldCache lookup -< a; case m of
      Just (s,b) -> do
        newCache write -< (a,b,s)
        returnA -< b
      Nothing -> do
        modify' (\((),cache) -> ((),cache { stable = Unstable })) -< ()
        newCache initialize -< a
  lookup = newCache lookup
  update = proc (a,b) -> do
    (s,a',b') <- newCache update -< (a,b)
    s' <- modify' (\(s,cache) -> let s' = stable cache ⊔ s in (s',cache { stable = s' })) -< s
    returnA -< (s',a',b')
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
  Monotone :: s -> HashMap a b -> Monotone (s,a) (s,b)

type instance Widening (Monotone (s,a) (s,b)) = (W.Widening s,W.Widening b)

instance IsEmpty s => IsEmpty (Monotone (s,a) (s,b)) where
  empty = Monotone empty empty

instance (Show s, Show a, Show b) => Show (Monotone (s,a) (s,b)) where
  show (Monotone s m) = show (s,m)

instance (Pretty s, Pretty a, Pretty b) => Pretty (Monotone (s,a) (s,b)) where
  pretty (Monotone s m) = vsep ["Monotone:" <+> pretty s, "NonMonotone:" <+> align (list [ pretty k <+> "->" <+> pretty v | (k,v) <- M.toList m])]

-- instance (ArrowChoice c, Profunctor c) => ArrowIterate (CacheT Monotone (s,a) (s,b) c) where
--   nextIteration = proc () -> modify' (\(_,Monotone s _) -> ((),Monotone s empty)) -< ()
--   isStable = proc _ -> returnA -< error "Don't use Monotone for parallel fixpoint iteration"
--   {-# INLINE nextIteration #-}
--   {-# INLINE isStable #-}

instance (Identifiable a, LowerBounded b, ArrowChoice c, Profunctor c) => ArrowCache (s,a) (s,b) (CacheT Monotone (s,a) (s,b) c) where
  initialize = CacheT $ modify' $ \((s,a),Monotone s' cache) ->
    let cache' = M.insertWith (\_ _old -> _old) a bottom cache
        b = M.lookupDefault bottom a cache
    in ((s,b),Monotone s' cache')
  lookup = CacheT $ modify' $ \((s,a),m@(Monotone _ cache)) ->
    ((\b -> (Unstable,(s,b))) <$> M.lookup a cache, m)
  update = CacheT $ askConst $ \(widenS,widenB) -> modify' $ \(((_,a),(sNew,b)),Monotone sOld cache) ->
    let (stable1,sWiden) = widenS sOld sNew
    in case M.lookup a cache of
        Just b' ->
          let ~(stable2,b'') = widenB b' b
          in ((stable1 ⊔ stable2, (sWiden,a), (sWiden,b'')), Monotone sWiden (M.insert a b'' cache))
        Nothing -> ((Unstable,(sWiden,a), (sWiden,b)),Monotone sWiden (M.insert a b cache))
  write = CacheT $ modify' $ \(((_, a), (_, b), _),Monotone s cache) -> ((),Monotone s (M.insert a b cache))
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

instance (Arrow c, Profunctor c) => ArrowIterateCache (CacheT Monotone (s,a) (s,b) c) where
  nextIteration = proc () -> modify' (\((),Monotone s _) -> ((),Monotone s empty)) -< ()
  {-# INLINE nextIteration #-}

------ Product Cache ------
data (**) cache1 cache2 a b where
  Product :: cache1 a1 b1 -> cache2 a2 b2 -> (**) cache1 cache2 (a1,a2) (b1,b2)

type instance Widening ((cache1 ** cache2) (a1,a2) (b1,b2)) = (Widening (cache1 a1 b1), Widening (cache2 a2 b2))

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
lubMaybe (Just (s1,b1), Just (s2,b2)) = Just (s1 ⊔ s2,(b1,b2))
lubMaybe _ = Nothing

lubStable :: ((Stable,a1,b1),(Stable,a2,b2)) -> (Stable,(a1,a2),(b1,b2))
lubStable ((s1,a1,b1),(s2,a2,b2)) = (s1 ⊔ s2,(a1,a2),(b1,b2))
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

-- instance (Arrow c, Profunctor c, ArrowIterate (CacheT cache a b c)) => ArrowIterate (CacheT (Context ctx cache) a b c) where
--   nextIteration = withCache nextIteration
--   isStable = withCache isStable
--   {-# INLINE nextIteration #-}
--   {-# INLINE isStable #-}

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

