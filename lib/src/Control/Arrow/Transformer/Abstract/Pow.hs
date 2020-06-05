{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Pow where

import           Prelude hiding (id,(.), fail,lookup)

import           Control.Arrow
import qualified Control.Arrow.Fix as Fix
import           Control.Arrow.Trans
import           Control.Arrow.Environment as Env
import           Control.Arrow.Store as Store
import           Control.Arrow.Closure as Cls
import           Control.Arrow.Fail as Fail
import           Control.Arrow.Fix.Context
import           Control.Category
import           Control.Arrow.Order

import           Data.Either
import           Data.Profunctor hiding (map')
import           Data.Abstract.Powerset as Pow

newtype PowT c x y = PowT (c (Pow x) (Pow y))

instance Fix.ArrowFix (Underlying (PowT c) x y) => Fix.ArrowFix (PowT c x y) where
  type Fix (PowT c x y) = Fix.Fix (Underlying (PowT c) x y)

instance ArrowLift (PowT c) where type Underlying (PowT c) x y = c (Pow x) (Pow y)

instance (ArrowRun c) => ArrowRun (PowT c) where type Run (PowT c) x y = Run c (Pow x) (Pow y)

instance Category c => Category (PowT c) where
  id = lift id
  -- gets used
  f . g = lift (unlift f . unlift g)
  {-# INLINE id #-}
  {-# INLINE (.) #-}

instance ArrowTrans PowT where
  lift' f = lift $ dimap unsafeExtract singleton f
  {-# INLINE lift' #-}

instance Profunctor c => Profunctor (PowT c) where
  dimap f g h = lift $ dimap (fmap f) (fmap g) (unlift h)
  lmap f h = lift $ lmap (fmap f) (unlift h)
  rmap g h = lift $ rmap (fmap g) (unlift h)
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

instance (Arrow c, Profunctor c) => Arrow (PowT c) where
  -- gets used 
  arr f = rmap f id
  first f = lift $ dimap Pow.unzip (\(xs,ys) -> crossproduct xs ys) (first $ unlift f)
  second f = lift $ dimap Pow.unzip (\(xs,ys) -> crossproduct xs ys) (second $ unlift f)
  f *** g = lift $ dimap Pow.unzip (\(xs,ys) -> crossproduct xs ys) (unlift f *** unlift g)
  f &&& g = lift $ rmap (\(xs,ys) -> crossproduct xs ys) (unlift f &&& unlift g)
  {-# INLINE arr #-}
  {-# INLINE first #-}
  {-# INLINE second #-}
  {-# INLINE (***) #-}
  {-# INLINE (&&&) #-}

instance (Profunctor c, ArrowChoice c) => ArrowChoice (PowT c) where
  left f = lift $ dimap partition1 repartition1 (first $ unlift f) -- COMONADS ?? 
  right f = lift $ dimap partition2 repartition2 (second $ unlift f)
  f ||| g = lift $ dimap partition (\(x,y) -> Pow.concat x y) (unlift f *** unlift g)
  f +++ g = left f >>> right g
  {-# INLINE left #-}
  {-# INLINE right #-}
  {-# INLINE (|||) #-}
  {-# INLINE (+++) #-}

instance (ArrowEnv var val c) => ArrowEnv var val (PowT c) where
  type Join y (PowT c) = Env.Join (Pow y) c
  lookup f g = lift $ lmap partition2' (Env.lookup (lmap crossproduct2 (unlift f)) (unlift g))
  -- gets used 
  extend f = lift $ lmap unsafeUnzip3 (Env.extend (unlift f))
  {-# INLINE lookup #-}
  {-# INLINE extend #-}

instance (ArrowStore var val c) => ArrowStore var val (PowT c) where
  type Join y (PowT c) = Store.Join (Pow y) c
  read f g = lift $ lmap partition2' (Store.read (lmap crossproduct2 (unlift f)) (unlift g))
  write = lift' Store.write
  {-# INLINE read #-}
  {-# INLINE write #-}

instance (ArrowClosure expr cls c) => ArrowClosure expr cls (PowT c) where
  type Join y cls (PowT c) = Cls.Join (Pow y) cls c
  apply f = lift $ lmap partition2' (Cls.apply (lmap crossproduct2 (unlift f)))
  {-# INLINE apply #-}

instance (ArrowFail e c, Profunctor c) => ArrowFail e (PowT c) where
  type Join y (PowT c) = Fail.Join y c
  fail = lift' fail
  {-# INLINE fail #-}

instance (Arrow c, Profunctor c) => ArrowLowerBounded y (PowT c) where
  bottom = lift $ arr (const Pow.empty)
  {-# INLINE bottom #-}

instance (Arrow c, Profunctor c, ArrowComplete (Pow y) c) => ArrowComplete y (PowT c) where
  f <⊔> g = lift $ unlift f <⊔> unlift g
  {-# INLINE (<⊔>) #-}

instance ArrowContext ctx c => ArrowContext ctx (PowT c) where
  localContext f = lift $ lmap partition2' (localContext $ unlift f)
  {-# INLINE localContext #-}

crossproduct2 :: (a, Pow b) -> Pow (a, b)
crossproduct2 (x, ys) = do 
  y <- ys
  return (x,y)
{-# INLINE crossproduct2 #-}

crossproduct3 :: (a, b, Pow c) -> Pow (a, b, c)
crossproduct3 (x, y, zs) = do 
  z <- zs 
  return (x, y, z)
{-# INLINE crossproduct3 #-}

unsafeUnzip3 :: Pow (a, b, c) -> (a, b, Pow c)
unsafeUnzip3 pow = let (xy,z) = Pow.unzip (fmap (\(a,b,c) -> ((a,b),c)) pow) in
  let (x,y) = Pow.unzip xy in (unsafeExtract x,unsafeExtract y,z)
{-# INLINE unsafeUnzip3 #-}

partition :: Pow (Either a b) -> (Pow a, Pow b)
partition x = (fmap leftOrError (Pow.filter isLeft x), fmap rightOrError (Pow.filter isRight x))
{-# INLINE partition #-}

-- is this sound? -> Left can't ever be empty, nor can have more than one unique element 
partition1 :: Pow (Either a b) -> (Pow a, b)
partition1 x = (fmap leftOrError (Pow.filter isLeft x), unsafeExtract $ fmap rightOrError (Pow.filter isRight x)) 
{-# INLINE partition1 #-}

repartition1 :: (Pow a, b) -> Pow (Either a b)
repartition1 (a, b) = Pow.push (Right b) (fmap Left a)
{-# INLINE repartition1 #-}

-- is this sound? -> Left can't ever be empty, nor can have more than one unique element 
partition2 :: Pow (Either a b) -> (a, Pow b)
partition2 x = (unsafeExtract $ fmap leftOrError (Pow.filter isLeft x), fmap rightOrError (Pow.filter isRight x))
{-# INLINE partition2 #-}

-- only sound if this is inverts a crossproduct with only one element as argument for a
partition2' :: Pow (a, b) -> (a, Pow b) 
partition2' x = let (a, b) = Pow.unzip x in (unsafeExtract a, b) 
{-# INLINE partition2' #-}

repartition2 :: (a, Pow b) -> Pow (Either a b)
repartition2 (a, b) = Pow.push (Left a) (fmap Right b)
{-# INLINE repartition2 #-}

leftOrError :: Either a b -> a 
leftOrError x = case x of Left y -> y 
                          _ -> error "expected left"
{-# INLINE leftOrError #-}

rightOrError :: Either a b -> b 
rightOrError x = case x of Right y -> y 
                           _ -> error "expected right"
{-# INLINE rightOrError #-}

unsafeExtract :: Pow a -> a 
unsafeExtract x = case Pow.size x of 1 -> Pow.index x 0 
                                     _ -> error "expected singleton"
{-# INLINE unsafeExtract #-}                                     