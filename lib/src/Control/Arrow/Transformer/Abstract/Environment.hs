{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
module Control.Arrow.Transformer.Abstract.Environment where

import           Prelude hiding ((.),read,Maybe(..))
import qualified Prelude as P 

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Cont
import           Control.Arrow.Const
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Reader as Reader
import           Control.Arrow.State
import           Control.Arrow.Store
import           Control.Arrow.Fail
import           Control.Arrow.Except
import           Control.Arrow.Trans
import           Control.Arrow.Environment as Env
import           Control.Arrow.Closure as Cls
import           Control.Arrow.Fix
import           Control.Arrow.Order
import           Control.Arrow.LetRec
import           Control.Arrow.Fix.Context 
import           Control.Arrow.Frame

import           Data.Abstract.IntersectionSet (Set)
import           Data.Abstract.Maybe
import qualified Data.Abstract.StrongMap as SM
import qualified Data.Abstract.Environment.Flat as FM
import           Data.Abstract.Closure (Closure)
import qualified Data.Abstract.Closure as Abs
import           Data.HashMap.Lazy(HashMap)  
import qualified Data.HashMap.Lazy as HM 
import           Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import           Data.Hashed.Lazy
import           Data.Order
import           Data.Identifiable
import           Data.Profunctor
import           Data.Profunctor.Unsafe((.#))
import           Data.Coerce

import GHC.Exts

newtype EnvT env c x y = EnvT (ReaderT env c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowLift,ArrowTrans,ArrowLowerBounded a,
            ArrowState s, ArrowFail e, ArrowExcept e, ArrowStore var' val', ArrowConst k, ArrowRun, ArrowCont,
            ArrowContext ctx)

runEnvT :: EnvT env c x y -> c (env,x) y
runEnvT = coerce
{-# INLINE runEnvT #-}

runEnvT' :: (IsList env, Item env ~ (var,val), Profunctor c) => EnvT env c x y -> c ([(var,val)],x) y
runEnvT' f = lmap (first fromList) (runEnvT f)
{-# INLINE runEnvT' #-}

instance (Arrow c, Profunctor c) => ArrowFrame env (EnvT env c) where
  newFrame (EnvT f) = EnvT $ Reader.local f
  askFrame = EnvT Reader.ask

instance (Identifiable var, UpperBounded val, ArrowChoice c, Profunctor c) => ArrowEnv var val (EnvT (SM.Map var val) c) where
  type Join y (EnvT (SM.Map var val) c) = ArrowComplete y c
  lookup (EnvT f) (EnvT g) = EnvT $ proc (var,x) -> do
    env <- Reader.ask -< ()
    case SM.lookup' var env of
      Just val        -> f -< (val,x)
      Nothing         -> g -< x
      JustNothing val -> (f -< (val,x)) <⊔> (g -< x)
  extend (EnvT f) = EnvT $ proc (var,val,x) -> do
    env <- Reader.ask -< ()
    Reader.local f -< (SM.insert var val env,x)
  vals = undefined
  {-# INLINE lookup #-}
  {-# INLINE extend #-}

instance (Identifiable var, Traversable val, Complete (val (Set var)), ArrowChoice c, Profunctor c) 
    => ArrowEnv var (val (FM.Env var val)) (EnvT (FM.Env var val) c) where
  type Join y (EnvT (FM.Env var val) c) = ArrowComplete y c
  lookup (EnvT f) (EnvT g) = EnvT $ proc (var,x) -> do
    env <- Reader.ask -< ()
    case FM.lookup var env of
      Just val        -> f -< (val,x)
      Nothing         -> g -< x
      JustNothing val -> (f -< (val,x)) <⊔> (g -< x)
  extend (EnvT f) = EnvT $ proc (var,val,x) -> do
    env <- Reader.ask -< ()
    Reader.local f -< (FM.insert var val env,x)
  vals = undefined
  {-# INLINE lookup #-}
  {-# INLINE extend #-}

instance (Identifiable var, Identifiable addr, ArrowChoice c, Profunctor c)
    => ArrowEnv var addr (EnvT (Hashed (HashMap var addr)) c) where
  type Join y (EnvT (Hashed (HashMap var addr)) c) = ()
  lookup (EnvT f) (EnvT g) = EnvT $ proc (var,x) -> do
    env <- Reader.ask -< ()
    case HM.lookup var (unhashed env) of
      P.Just addr -> f -< (addr,x)
      P.Nothing   -> g -< x
  extend (EnvT f) = EnvT $ proc (var,addr,x) -> do
    env <- Reader.ask -< ()
    Reader.local f -< (mapHashed (HM.insert var addr) env, x)
  vals = EnvT $ proc _ -> do
    env <- Reader.ask -< () 
    returnA -< Set.fromList $ HM.elems $ unhashed env

  {-# INLINE lookup #-}
  {-# INLINE extend #-}
  {-# SCC lookup #-}
  {-# SCC extend #-}

instance (Identifiable var, Traversable val, Complete (val (Set var)), ArrowChoice c, Profunctor c) => ArrowLetRec var (val (FM.Env var val)) (EnvT (FM.Env var val) c) where
  letRec (EnvT f) = EnvT $ proc (ls,x) -> do
    env <- Reader.ask -< ()
    Reader.local f -< (FM.insertRec ls env,x)
  {-# INLINE letRec #-}

instance (Identifiable expr, ArrowChoice c, Profunctor c) => ArrowClosure expr (Closure expr env) (EnvT env c) where
  type Join y (Closure expr env) (EnvT env c) = ArrowComplete y c
  closure = EnvT $ proc expr -> do
    env <- Reader.ask -< ()
    returnA -< Abs.closure expr env
  apply (EnvT f) = EnvT $ Abs.apply $ proc ((expr,env),x) ->
    Reader.local f -< (env,(expr,x))
  {-# INLINE closure #-}
  {-# INLINE apply #-}

instance (Identifiable expr, ArrowChoice c, Profunctor c) =>
  ArrowClosure expr (Closure expr (HashSet (Hashed (HashMap var addr)))) (EnvT (Hashed (HashMap var addr)) c) where
  type Join y (Closure expr (HashSet (Hashed (HashMap var addr)))) (EnvT (Hashed (HashMap var addr)) c) = Complete y
  closure = EnvT $ proc expr -> do
    env <- Reader.ask -< ()
    returnA -< Abs.closure expr (Set.singleton env)
  apply (EnvT f) = Abs.apply $ proc ((expr,envs),x) ->
    (| joinList (error "encountered an empty set of environments" -< ())
                (\env -> EnvT (Reader.local f) -< (env,(expr,x))) |) (Set.toList envs)
  {-# INLINE closure #-}
  {-# INLINE apply #-}
  {-# SCC closure #-}
  {-# SCC apply #-}

instance (ArrowApply c, Profunctor c) => ArrowApply (EnvT env c) where
  app = EnvT (app .# first coerce)
  {-# INLINE app #-}

instance ArrowReader r c => ArrowReader r (EnvT env c) where
  ask = lift' Reader.ask
  local f = lift $ lmap (\(env,(r,x)) -> (r,(env,x))) (Reader.local (unlift f))
  {-# INLINE ask #-}
  {-# INLINE local #-}

instance ArrowFix (Underlying (EnvT env c) x y) => ArrowFix (EnvT env c x y) where
  type Fix (EnvT env c x y) = Fix (Underlying (EnvT env c) x y)

instance (Complete y, Arrow c, Profunctor c) => ArrowComplete y (EnvT env c) where
  EnvT f <⊔> EnvT g = EnvT (rmap (uncurry (⊔)) (f &&& g))
  {-# INLINE (<⊔>) #-}