{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE PatternSynonyms #-}
module Control.Arrow.Transformer.Abstract.ReachingDefinitions(
  ReachingDefinitions,
  pattern ReachingDefs,
  runReachingDefs,
  runReachingDefs',
  ReachingDefs
) where

import           Prelude hiding ((.),read,lookup)
import           Control.Category
import           Control.Arrow
import           Control.Arrow.Environment
import           Control.Arrow.Fix
import           Control.Arrow.Lift
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Fail
import           Control.Arrow.Store
import           Control.Arrow.Effect
import           Control.Arrow.Transformer.Effect
import           Control.Arrow.Transformer.ForwardAnalysis

import           Data.Identifiable
import qualified Data.HashSet as H

type PointsTo v l = (v,Maybe l)
type ReachingDefs v l = Forward (ReachingDef v l)

newtype ReachingDefinitions v l c x y = ReachingDefinitions (ForwardAnalysis (ReachingDef v l) c x y)

{-# COMPLETE ReachingDefs #-}
pattern ReachingDefs :: c (ReachingDefs v l,x) (ReachingDefs v l,(ReachingDefs v l,y)) -> ReachingDefinitions v l c x y
pattern ReachingDefs f = ReachingDefinitions (Effect f)

runReachingDefs :: Arrow c => ReachingDefinitions v l c x y -> c (ReachingDefs v l,x) y
runReachingDefs (ReachingDefinitions f) = runForwardAnalysis f

runReachingDefs' :: Arrow c => ReachingDefinitions v l c x y -> c (ReachingDefs v l,x) (ReachingDefs v l,(ReachingDefs v l,y))
runReachingDefs' (ReachingDefs f) = f

instance (Identifiable var, Identifiable lab, ArrowStore var val lab c)
  => ArrowStore var val lab (ReachingDefinitions var lab c) where
  read = lift read 
  write = ReachingDefinitions $ proc (x,v,l) -> do
    record (\(x,l) (defs) -> H.insert (x,Just l) (H.filter (\(y,_) -> x /= y) defs)) -< (x,l)
    lift write -< (x,v,l)

-- val (+): (Int,Int) => Int
-- val f: Int :=>  String
-- arrows (a) {
--   val x = a + 1
--   val y = f(5)
-- }
-- arr (
-- x <- arr (\a -> a + 1) -< a

instance (Identifiable var, Identifiable lab, ArrowEnv var val env c)
  => ArrowEnv var val env (ReachingDefinitions var lab c) where
  lookup = lift lookup
  getEnv = lift getEnv
  extendEnv = ReachingDefinitions $ proc (x,v,env) -> do
    record (\x defs -> H.insert (x,Nothing) (H.filter (\(y,_) -> x /= y) defs)) -< x -- TODO: provide label
    lift extendEnv -< (x,v,env)
  localEnv (ReachingDefinitions f) = ReachingDefinitions $ proc (env,a) -> do -- :: c a b -> c (env,a) b
    vars <- lift getEnvDomain -< env
    recordLocal
      (\vs _ -> H.fromList $ Prelude.map (\v -> (v,Nothing)) vs)
      lift (localEnv f)
      -< (vars, (env,a))
    -- TODO unmark reachability of `vars`
  getEnvDomain = lift getEnvDomain

type instance Fix x y (ReachingDefinitions v l c) = ReachingDefinitions v l (Fix x y (ForwardAnalysis (ReachingDef v l) c))
deriving instance (Arrow c, ArrowFix x y (ForwardAnalysis (ReachingDef v l) c)) => ArrowFix x y (ReachingDefinitions v l c)

instance (ArrowApply c) => ArrowApply (ReachingDefinitions v l c) where
  app = ReachingDefinitions ((\(ReachingDefinitions f,x) -> (f,x)) ^>> app)

deriving instance ArrowLift (ReachingDefinitions v l)
deriving instance Arrow c => Category (ReachingDefinitions v l c)
deriving instance Arrow c => Arrow (ReachingDefinitions v l c)
deriving instance ArrowChoice c => ArrowChoice (ReachingDefinitions v l c)
deriving instance ArrowReader r c => ArrowReader r (ReachingDefinitions v l c)
deriving instance ArrowFail e c => ArrowFail e (ReachingDefinitions v l c)
deriving instance ArrowState s c => ArrowState s (ReachingDefinitions v l c)
