{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.GlobalState where

import           Control.Arrow
import           Control.Arrow.Trans

import           Control.Arrow.Transformer.Concrete.Except as CE
import           Control.Arrow.Transformer.Abstract.Except as AE
import           Control.Arrow.Transformer.Kleisli
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.Stack
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Writer

import qualified Data.Order as O
import           Data.Profunctor

import           Language.Wasm.Structure hiding (exports)
import           Language.Wasm.Interpreter (ModuleInstance(..))




class ArrowGlobalState v m c | c -> v, c -> m where
  -- | Reads a global variable. Cannot fail due to validation.
  readGlobal :: c Int v
  -- | Writes a global variable. Cannot fail due to validation.
  writeGlobal :: c (Int, v) ()

  -- | Reads a function. Cannot fail due to validation.
  --readFunction :: c ((FuncType, ModuleInstance, Function), x) y -> c ((FuncType, HostFunction v c), x) y -> c (Int, x) y

  readFunction :: c ((FuncType, ModuleInstance, Function), x) y -> c (Int, x) y

  -- | readTable f g h (ta,ix,x)
  -- | Lookup `ix` in table `ta` to retrieve the function address `fa`.
  -- | Invokes `f (fa, x)` if all goes well.
  -- | Invokes `g (ta,ix,x)` if `ix` is out of bounds.
  -- | Invokes `h (ta,ix,x)` if `ix` cell is uninitialized.
  readTable :: c (Int,x) y -> c (Int,Int,x) y -> c (Int,Int,x) y -> c (Int,Int,x) y

  fetchMemory :: c Int m
  storeMemory :: c (Int, m) ()

  -- | Executes a function relative to a memory instance. The memory instance exists due to validation.
  -- withMemoryInstance :: c x y -> c (Int,x) y

deriving instance (ArrowGlobalState v m c) => ArrowGlobalState v m (ValueT v2 c)
instance (Profunctor c, Arrow c, ArrowGlobalState v m c) => ArrowGlobalState v m (StateT s c) where
    readGlobal = lift' readGlobal
    writeGlobal = lift' writeGlobal
    -- readFunction :: (StateT s c) (f, x) y -> (StateT s c) (Int, x) y
    -- a :: (StateT s c) (f, x) y
    -- unlift a :: Underlying (StateT s c) (f, x) y
    --             = c (s, (f, x)) (s, y)
    -- readFunction :: c (f, x) y -> c (Int, x) y
    -- need foo :: c (s, (f, x)) (s, y) -> c (s, (Int, x)) (s, y)
    -- lift :: c (s, (Int, x)) (s, y) ->
    --         StateT s c (Int, x) y
    --
    -- arr :: c (s, (f,x)) (s, y)
    -- flip a :: (StateT s c) ((f,x),s) (y,s)
    -- modify (flip a) :: (StateT s c) (f,x) y
    -- readFunction (modify (flip a)) :: c (Int, x) y
    -- second (readFunction (modify (flip a))) :: c (s, (Int,x)) (s, y)
    readFunction a = lift $ transform (unlift a)
--        -- proc (f,x) -> arr -< (s, (f,x)) :: c (f,x) (s, y)
--        -- ((proc (f,x) -> arr -< (s, (f,x))) >>^ snd) :: c (f,x) y
--        -- readFunction ((proc (f,x) -> arr -< (s, (f,x))) >>^ snd) :: c (Int, x) y
--        -- c (s, (Int, x)) y
        where transform f = proc (s, (i,x)) -> readFunction (proc (f,(s2,x)) -> f -< (s2, (f,x))) -< (i,(s,x))
--                   -- arr :: c (s, (f,x)) (s, y)
--                   -- proc (f,(s2,x)) -> arr -< (s2, (f,x)) :: c (f,(s,x)) (s, y)
--                   -- ((proc (f,(s2,x)) -> arr -< (s2, (f,x))) >>^ snd) :: c (f,(s,x)) y
--                   -- func "" :: c (Int, (s,x)) y (for z = (s x))
                                 --(sNew,y) <- readFunction ((proc (f,(s2,x)) -> arr -< (s2, (f,x)))) -< (i,(s,x))
                                 --returnA -< (sNew,y)


--        proc (s, (i,x)) -> do
--                            y <- func ((proc (f,(s2,x)) -> arr -< (s2, (f,x))) >>^ snd) -< (s,(i,x))
--                            returnA -< (s,y)
    fetchMemory = lift' fetchMemory
    storeMemory = lift' storeMemory

--foo2 :: (c (f,x) y -> c (Int,x) y) -> c (s, (f,x)) (s,y) -> c (s, (Int,x)) (s,y)
--foo2 func arr = proc (s, (i,x)) -> do
--                    y <- func ((proc (f,x) -> arr -< (s,(f,x))) >>^ snd) -< (i,x)
--                    returnA -< (s,y)
--
--foo :: (Arrow c) => (c (f, (s,x)) y -> c (Int, (s,x)) y) -> c (s, (f,x)) (s, y) -> c (s, (Int, x)) (s, y)
--foo func arr = proc (s, (i,x)) -> do
--                   -- arr :: c (s, (f,x)) (s, y)
--                   -- proc (f,(s2,x)) -> arr -< (s2, (f,x)) :: c (f,(s,x)) (s, y)
--                   -- ((proc (f,(s2,x)) -> arr -< (s2, (f,x))) >>^ snd) :: c (f,(s,x)) y
--                   -- func "" :: c (Int, (s,x)) y (for z = (s x))
--                   y <- func ((proc (f,(s2,x)) -> arr -< (s2, (f,x))) >>^ snd) -< (i,(s,x))
--                   returnA -< (s,y)

deriving instance (Arrow c, Profunctor c, ArrowGlobalState v m c) => ArrowGlobalState v m (CE.ExceptT e c)
deriving instance (O.Complete e, Arrow c, Profunctor c, ArrowGlobalState v m c) => ArrowGlobalState v m (AE.ExceptT e c)
deriving instance (Arrow c, Profunctor c, ArrowGlobalState v m c) => ArrowGlobalState v m (StackT s c)
instance (Monad f, Arrow c, Profunctor c, ArrowGlobalState v m c) => ArrowGlobalState v m (KleisliT f c) where
    readGlobal = lift' readGlobal
    writeGlobal = lift' writeGlobal
    readFunction a = lift (readFunction (unlift a))
    fetchMemory = lift' fetchMemory
    storeMemory = lift' storeMemory
instance (Arrow c, Profunctor c, ArrowGlobalState v m c) => ArrowGlobalState v m (ReaderT r c) where
    readGlobal = lift' readGlobal
    writeGlobal = lift' writeGlobal
    -- unlift arr :: c (r, (f,x)) y
    -- lift :: c (r, (Int,x)) y -> c (Int,x) y
    -- transform :: c (r, (f,x)) y -> c (r, (Int,x)) y
    -- readFunction :: c (f,x) y -> c (Int,x) y
    readFunction a = lift $ transform (unlift a)
        where transform f = proc (r, (i,x)) ->
                                readFunction (proc (f,(r,x)) -> f -< (r, (f,x))) -< (i,(r,x))
    fetchMemory = lift' fetchMemory
    storeMemory = lift' storeMemory

instance (Arrow c, Profunctor c, Monoid w, ArrowGlobalState v m c) => ArrowGlobalState v m (WriterT w c) where
    readGlobal = lift' readGlobal
    writeGlobal = lift' writeGlobal
    readFunction a = lift (readFunction (unlift a))
    fetchMemory = lift' fetchMemory
    storeMemory = lift' storeMemory
