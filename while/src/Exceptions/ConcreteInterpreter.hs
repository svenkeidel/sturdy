{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Concrete interpreter of the While language.
module Exceptions.ConcreteInterpreter where

import           Prelude hiding (read,fail,id,(.))

import           Exceptions.Syntax
import           ConcreteInterpreter
import           Exceptions.GenericInterpreter
import qualified Exceptions.GenericInterpreter as Generic

import           Data.Concrete.Error (Error)
import           Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Text (Text)
import           Data.Label

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Transformer.Concrete.Except
import           Control.Arrow.Transformer.Concrete.Failure
import           Control.Arrow.Transformer.Concrete.Environment
import           Control.Arrow.Transformer.Concrete.Random
import           Control.Arrow.Transformer.Concrete.Store

import qualified System.Random as R

-- | Values of the While language can be booleans or numbers.
type Exception = (Text,Val)

-- | The concrete interpreter of the while language instantiates
-- 'Generic.run' with the concrete components for failure ('FailureT'), store ('StoreT'),
-- environments ('EnvT'), random numbers ('RandomT'), and values ('ConcreteT').
run :: [LStatement] -> Error String (Error Exception (HashMap Addr Val))
run ss =
  fmap (fmap fst) $
    runFailureT
      (runExceptT
        (runStoreT
          (runEnvT
            (runRandomT
               (runConcreteT
                 (Generic.run ::
                   ConcreteT
                     (RandomT
                       (EnvT Text Addr
                         (StoreT Addr Val
                           (ExceptT Exception
                             (FailureT String
                               (->)))))) [Statement] ()))))))
      (M.empty,(M.empty,(R.mkStdGen 0, generate <$> ss)))

instance ArrowChoice c => IsException Exception Val (ConcreteT c) where
  type JoinExc (ConcreteT c) x y = ()
  namedException = id
  matchException f g = proc (name,(name',v),x) ->
    if (name == name')
       then f -< (v,x)
       else g -< x
