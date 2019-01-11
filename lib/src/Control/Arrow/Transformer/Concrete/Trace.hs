{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Transformer.Concrete.Trace where

import           Prelude hiding ((.))

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Arrow.Writer
import           Control.Arrow.Transformer.Writer

import           Data.Sequence (Seq)
import qualified Data.Sequence as S

data Entry a b = Call a | Return b deriving (Show,Eq)
type Log a b = Seq (Entry a b)

newtype TraceT a b c x y = TraceT (WriterT (Log a b) c x y)

runTraceT :: TraceT a b c x y -> c x (Log a b,y)
runTraceT (TraceT (WriterT f)) = f

deriving instance Arrow c => Category (TraceT a b c)
deriving instance Arrow c => Arrow (TraceT a b c)
deriving instance ArrowChoice c => ArrowChoice (TraceT a b c)
instance ArrowApply c => ArrowApply (TraceT a b c) where
  app = TraceT $ (\(TraceT f,x) -> (f,x)) ^>> app

instance ArrowFix x (Log x y,y) c => ArrowFix x y (TraceT x y c) where
  fix f = TraceT $ fix (unwrap . f . TraceT)
    where
      unwrap :: Arrow c => TraceT x y c x y -> WriterT (Log x y) c x y
      unwrap (TraceT g) = proc x -> do
        tell -< S.singleton (Call x)
        y <- g -< x
        tell -< S.singleton (Return y)
        returnA -< y
    
