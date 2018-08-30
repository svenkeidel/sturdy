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

newtype Trace a b c x y = Trace (Writer (Log a b) c x y)

runTrace :: Trace a b c x y -> c x (Log a b,y)
runTrace (Trace (Writer f)) = f

deriving instance Arrow c => Category (Trace a b c)
deriving instance Arrow c => Arrow (Trace a b c)
deriving instance ArrowChoice c => ArrowChoice (Trace a b c)
instance ArrowApply c => ArrowApply (Trace a b c) where
  app = Trace $ (\(Trace f,x) -> (f,x)) ^>> app

instance ArrowFix x (Log x y,y) c => ArrowFix x y (Trace x y c) where
  fix f = Trace $ fix (unwrap . f . Trace)
    where
      unwrap :: Arrow c => Trace x y c x y -> Writer (Log x y) c x y
      unwrap (Trace g) = proc x -> do
        tell -< S.singleton (Call x)
        y <- g -< x
        tell -< S.singleton (Return y)
        returnA -< y
    
