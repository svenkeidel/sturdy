{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Cont where

import Control.Arrow
import Data.Profunctor
import Data.Kind

-- | Arrow type class that gives access to the current continuation and that allows to jump to a continuation.
--
-- For example, you can use this type class like this to implement a while loop, which
-- takes two continuations, @break@ that jumps out of the loop and @continue@
-- that jumps out of the current iteration:
--
-- > while :: (ArrowCont r c, Arrow c) => (c () r -> c y r -> c x ()) -> c x y
-- > while body = callCC (\break -> loop break)
-- >   where
-- >     loop break = proc x -> do
-- >       callCC (\continue -> body continue break) -< x
-- >       loop break -< x
class (Arrow c, Profunctor c) => ArrowCont c where
  type Cont c y :: Type

  -- | This operation exposes the current continuation. The continuation can be
  -- used to escape the current context.
  callCC :: (Cont c y -> c x y) -> c x y

  -- | Jump to a continuation. This escapes the context to the point where the continuation was created.
  jump :: Cont c x -> c x y
