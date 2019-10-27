module Data.Abstract.IndirectMap where

-- import           Data.HashMap.Lazy(HashMap)
-- import qualified Data.HashMap.Lazy as M

-- newtype Map x y z = Map (HashMap x y, HashMap y z)

-- instance (Identifiable var, Identifiable addr, Complete val, ArrowChoice c, Profunctor c) =>
--   ArrowEnv var val (EnvT var addr val c) where
--   type Join y (EnvT var addr val c) = ()
--   lookup (EnvT f) (EnvT g) = EnvT $ proc (var,x) -> do
--     (env,store) <- Reader.ask -< ()
--     case do { addr <- HM.lookup var env; HM.lookup addr store } of
--       Just val -> f -< (val,x)
--       Nothing  -> g -< x
--   extend (EnvT f) = EnvT $ ConstT $ StaticT $ \alloc -> ReaderT $ proc ((env,store),(var,val,x)) -> do
--     addr <- alloc -< (var,val,(env,store))
--     let env'   = HM.insert var addr env
--         store' = HM.insertWith (⊔) addr val store
--     runReaderT (runConstT alloc f) -< ((env',store'),x)
