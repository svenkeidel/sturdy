module Utils where

import Control.Arrow
import Control.Monad.Trans.Reader

newtype MaybeArrow c x y = MaybeArrow { runMaybeArrow :: c x (Maybe y) }

--localA :: Interp a b -> Interp (Env, a) b
localA :: Kleisli (ReaderT env m) a b -> Kleisli (ReaderT env m) (env, a) b
localA (Kleisli inner) = Kleisli (\(env, a) -> local (const env) $ inner a)
