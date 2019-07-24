{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module FiniteMapSpec where

import Prelude hiding (lookup,id)

import Control.Arrow
import Control.Arrow.Environment
import Control.Arrow.State
import Control.Arrow.Transformer.Abstract.BoundedEnvironment
import Control.Arrow.Transformer.Abstract.Failure
import Control.Arrow.Transformer.State

import Data.Abstract.Interval
import Data.Abstract.Failure
import Data.Order
import Data.Text (Text)

import Test.Hspec

main :: IO ()
main = hspec spec

type Val = Interval Int
type Addr = Int
type Ar = EnvT Text Addr Val (StateT Addr (FailureT String (->)))

spec :: Spec
spec = do
  it "env with allocation strategy (addr+1)%5" $ do
    let setup :: Ar Text Val
        setup = proc x -> do
          extend' lookup' -< ([("a",1), ("b",2), ("c",3), ("d",4), ("e",5), ("f",6), ("g",7)],x)
  
    runTests setup "a" `shouldBe` Success (Interval 1 6)
    runTests setup "b" `shouldBe` Success (Interval 2 7)
    runTests setup "c" `shouldBe` Success (Interval 3 3)
    runTests setup "d" `shouldBe` Success (Interval 4 4)
    runTests setup "e" `shouldBe` Success (Interval 5 5)
    runTests setup "f" `shouldBe` Success (Interval 1 6)
    runTests setup "g" `shouldBe` Success (Interval 2 7)

  where
    runTests s x = runFailureT (evalStateT (runEnvT alloc s) ) (0,([],x))

    alloc :: StateT Addr (FailureT String (->)) (Text,val,env) Addr 
    alloc = proc _ -> do
      addr <- get -< ()
      put -< (succ addr `mod` 5)
      returnA -< addr

instance Complete Int where (⊔) = undefined
