{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Environment where

import           Prelude hiding (lookup,fail,id)

import qualified Control.Comonad as C
import           Control.Category
import           Control.Arrow hiding (ArrowMonad)
import           Control.Arrow.Monad
import           Control.Arrow.Trans
import           Control.Arrow.Transformer.Cokleisli
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Kleisli
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Static
import           Control.Arrow.Transformer.Writer
import           Control.Arrow.Fail (ArrowFail(fail))
import qualified Control.Arrow.Fail as Fail

import           Data.String
import           Data.Profunctor
import           Data.Monoidal

import           Text.Printf

import           GHC.Exts (Constraint)


-- | Arrow-based interface for interacting with environments.
class (Arrow c, Profunctor c) => ArrowEnv var val c | c -> var, c -> val where
  -- | Type class constraint used by the abstract instances to join arrow computations.
  type family Join y c :: Constraint

  -- | Lookup a variable in the current environment. If the
  -- environment contains a binding of the variable, the first
  -- continuation is called and the second computation otherwise.
  lookup :: Join y c => c (val,x) y -> c x y -> c (var,x) y

  -- | Extend an environment with a binding.
  extend :: c x y -> c (var,val,x) y

-- | Simpler version of environment lookup.
lookup' :: (Join val c, Fail.Join val c, Show var, IsString e, ArrowFail e c, ArrowEnv var val c) => c var val
lookup' = lookup'' id
{-# INLINE lookup' #-}

lookup'' :: (Join y c, Fail.Join y c, Show var, IsString e, ArrowFail e c, ArrowEnv var val c) => c val y -> c var y
lookup'' f = proc var ->
  lookup
    (proc (val,_) -> f     -< val)
    (proc var     -> fail  -< fromString $ printf "Variable %s not bound" (show var))
    -< (var,var)
{-# INLINE lookup'' #-}

extend' :: (ArrowChoice c, ArrowEnv var val c) => c x y -> c ([(var,val)],x) y
extend' f = proc (l,x) -> case l of
  ((var,val):l') -> extend (extend' f) -< (var,val,(l',x))
  [] -> f -< x
{-# INLINABLE extend' #-}

------------- Instances --------------
instance (ArrowComonad f c, ArrowEnv x y c) => ArrowEnv x y (CokleisliT f c) where
  type Join y (CokleisliT f c) = Join y c
  lookup f g = lift $ lmap costrength2 (lookup (lmap strength2 (unlift f)) (unlift g))
  extend f = lift $ lmap (\m -> let (x,y,_) = C.extract m in (x,y,fmap (\(_,_,z) -> z) m)) (extend (unlift f))
  {-# INLINE lookup #-}
  {-# INLINE extend #-}

instance ArrowEnv var val c => ArrowEnv var val (ConstT r c) where
  type Join y (ConstT r c) = Join y c
  lookup f g = lift $ \r -> lookup (unlift f r) (unlift g r)
  extend f = lift $ \r -> extend (unlift f r)
  {-# INLINE lookup #-}
  {-# INLINE extend #-}

instance (ArrowMonad f c, ArrowEnv x y c) => ArrowEnv x y (KleisliT f c) where
  type Join y (KleisliT f c) = Join (f y) c
  lookup f g = lift $ lookup (unlift f) (unlift g)
  extend f = lift $ extend (unlift f)
  {-# INLINE lookup #-}
  {-# INLINE extend #-}

instance ArrowEnv var val c => ArrowEnv var val (ReaderT r c) where
  type Join y (ReaderT r c) = Join y c
  lookup f g = lift $ lmap shuffle1
                    $ lookup (lmap shuffle1 (unlift f)) (unlift g)
  extend f = lift $ lmap (\(r,(var,val,x)) -> (var,val,(r,x))) (extend (unlift f))
  {-# INLINE lookup #-}
  {-# INLINE extend #-}

instance (ArrowEnv var val c) => ArrowEnv var val (StateT s c) where
  type Join y (StateT s c) = Join (s,y) c
  lookup f g = lift $ lmap shuffle1
                    $ lookup (lmap shuffle1 (unlift f))
                             (unlift g)
  extend f = lift $ lmap (\(s,(var,val,x)) -> (var,val,(s,x))) (extend (unlift f))
  {-# INLINE lookup #-}
  {-# INLINE extend #-}

instance (Applicative f, ArrowEnv var val c) => ArrowEnv var val (StaticT f c) where
  type Join y (StaticT f c) = Join y c
  lookup (StaticT f) (StaticT g) = StaticT $ lookup <$> f <*> g
  extend (StaticT f) = StaticT $ extend <$> f
  {-# INLINE lookup #-}
  {-# INLINE extend #-}
  {-# SPECIALIZE instance ArrowEnv var val c => ArrowEnv var val (StaticT ((->) r) c) #-}

instance (Monoid w, ArrowEnv var val c) => ArrowEnv var val (WriterT w c) where
  type Join y (WriterT w c) = Join (w,y) c
  lookup f g = lift $ lookup (unlift f) (unlift g)
  extend f = lift $ extend (unlift f)
  {-# INLINE lookup #-}
  {-# INLINE extend #-}
