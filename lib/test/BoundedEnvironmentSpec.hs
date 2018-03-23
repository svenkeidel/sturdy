{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module BoundedEnvironmentSpec where

import           Prelude hiding (lookup)

import           Control.Arrow
import           Control.Arrow.Environment
import           Control.Arrow.State
import           Control.Arrow.Transformer.Abstract.BoundedEnvironment
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.State

import           Data.Abstract.Interval
import           Data.Abstract.Error
import           Data.Text (Text)

import           Test.Hspec

main :: IO ()
main = hspec spec

type Val = Interval Int
type Addr = Int
type Ar = Environment Text Addr Val (StateArrow Addr (ErrorArrow String (->)))

instance ArrowAlloc Text Addr Val (StateArrow Addr (ErrorArrow String (->))) where
  alloc = proc _ -> do
    addr <- getA -< ()
    putA -< (succ addr `mod` 5)
    returnA -< addr

spec :: Spec
spec = do
  context "env = [a -> 1, b -> 2, c -> 3, d -> 4, e -> 5, f -> 6, g -> 7] with allocation strategy (addr+1)%5" $ do
    let setup :: Ar Text Val
        setup = proc x -> do
          env0 <- getEnv -< ()
          env1 <- extendEnv -< ("a",1,env0)
          env2 <- extendEnv -< ("b",2,env1)
          env3 <- extendEnv -< ("c",3,env2)
          env4 <- extendEnv -< ("d",4,env3)
          env5 <- extendEnv -< ("e",5,env4)
          env6 <- extendEnv -< ("f",6,env5)
          env7 <- extendEnv -< ("g",7,env6)
          localEnv lookup -< (env7,x)
  
    it "env(a) = [1,6]" $ runTests setup "a" `shouldBe` Success (Interval 1 6)
    it "env(b) = [2,6]" $ runTests setup "b" `shouldBe` Success (Interval 2 7)
    it "env(c) = [3,3]" $ runTests setup "c" `shouldBe` Success (Interval 3 3)
    it "env(d) = [4,4]" $ runTests setup "d" `shouldBe` Success (Interval 4 4)
    it "env(e) = [5,5]" $ runTests setup "e" `shouldBe` Success (Interval 5 5)
    it "env(f) = [1,6]" $ runTests setup "f" `shouldBe` Success (Interval 1 6)
    it "env(g) = [2,7]" $ runTests setup "g" `shouldBe` Success (Interval 2 7)

  context "env = [a -> 1, d -> 4, g -> 7] with allocation strategy (addr+1)%5" $ do
    let setup :: Ar Text Val
        setup = proc x -> do
          env0 <- getEnv -< ()
          env1 <- extendEnv -< ("a",1,env0)
          localEnv
            (proc () -> do
               env1 <- getEnv -< ()
               env2 <- extendEnv -< ("b",2,env1)
               extendEnv -< ("c",3,env2))
            -< (env1,())
          env4 <- extendEnv -< ("d",4,env1)
          localEnv
            (proc () -> do
               env4 <- getEnv -< ()
               env5 <- extendEnv -< ("e",5,env4)
               extendEnv -< ("f",6,env5))
            -< (env4,())
          env7 <- extendEnv -< ("g",7,env4)
          localEnv lookup -< (env7,x)
  
    it "env(a) = [1,6]" $ runTests setup "a" `shouldBe` Success (Interval 1 1)
    it "env(b) = Nothing" $ runTests setup "b" `shouldBe` Fail "variable \"b\" not found"
    it "env(c) = Nothing" $ runTests setup "c" `shouldBe` Fail "variable \"c\" not found"
    it "env(d) = [4,4]" $ runTests setup "d" `shouldBe` Success (Interval 4 4)
    it "env(e) = Nothing" $ runTests setup "e" `shouldBe` Fail "variable \"e\" not found"
    it "env(f) = Nothing" $ runTests setup "f" `shouldBe` Fail "variable \"f\" not found"
    it "env(g) = [2,7]" $ runTests setup "g" `shouldBe` Success (Interval 7 7)

  where
    runTests s x = runErrorArrow(evalState (runEnvironment s) ) (0,([],x))
