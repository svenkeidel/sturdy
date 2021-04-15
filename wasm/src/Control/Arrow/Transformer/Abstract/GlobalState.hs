{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Transformer.Abstract.GlobalState where

import           Prelude hiding (read)

-- import           Abstract

-- import           Control.Arrow
-- import           Control.Arrow.Const
-- import           Control.Arrow.Logger
-- import           Control.Arrow.Except
-- import           Control.Arrow.Fail
-- import           Control.Arrow.Fix
-- import           Control.Arrow.MemAddress
-- import           Control.Arrow.Memory
-- import           Control.Arrow.MemSizable
-- import           Control.Arrow.Reader
-- import           Control.Arrow.Serialize
-- import           Control.Arrow.Stack
-- import           Control.Arrow.State
-- import           Control.Arrow.Store
-- import           Control.Arrow.Trans
-- import           Control.Arrow.GlobalState
-- import           Control.Arrow.WasmFrame

-- import           Control.Arrow.Transformer.State
-- import           Control.Arrow.Transformer.Abstract.Store

-- import           Control.Category

-- import           Data.Abstract.FreeCompletion
-- import           Data.Abstract.Map (Map)

-- import           Data.Profunctor

--newtype GlobalStateT v storeV c x y = GlobalStateT (StateT (GlobalState v) (StoreT (Map (Int,v) storeV) c) x y)
--    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift, ArrowReader r,
--              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowRun, ArrowFrame fd val,
--              ArrowStack st, ArrowLogger l, ArrowSerialize val dat valTy datDecTy datEncTy)
--
--instance (ArrowState s c) => ArrowState s (GlobalStateT v storeV c) where
--    -- TODO
--
--instance ArrowTrans (GlobalStateT v storeV) where
--    lift' a = GlobalStateT (lift' (lift' a))
--
--instance (Arrow c, Profunctor c) => ArrowGlobalState v Int (GlobalStateT v storeV c) where
--    fetchMemory = arr Prelude.id
--    storeMemory = arr $ const ()
--
--instance (ArrowChoice c) => ArrowMemory Int Value Value (GlobalStateT BaseValue Value c) where
--    memread (GlobalStateT sCont) (GlobalStateT eCont) = GlobalStateT $ proc (i, (addr, size, x)) -> do
--        case addr of
--            Lower a -> do
--                y <- read sCont eCont -< ((i,a),x)
--                returnA -< (i,y)
--            Top -> returnA -< error "TODO: join over all memory cells"
