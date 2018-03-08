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

import           Data.Interval
import           Data.Text (Text)

import           Test.Hspec

main :: IO ()
main = hspec spec

type Val = Interval Int
type Addr = Int
type Ar = BoundedEnv Text Addr Val (StateArrow Addr (->))

instance ArrowAlloc Text Addr Val (StateArrow Addr (->)) where
  alloc = proc _ -> do
    addr <- getA -< ()
    putA -< (succ addr `mod` 5)
    returnA -< addr

spec :: Spec
spec = do
  context "env = [a -> 1, b -> 2, c -> 3, d -> 4, e -> 5, f -> 6, g -> 7] with allocation strategy (addr+1)%5" $ do
    let setup :: Ar x y -> Ar x y
        setup f = proc x -> do
          env0 <- getEnv -< ()
          env1 <- extendEnv -< ("a",1,env0)
          env2 <- extendEnv -< ("b",2,env1)
          env3 <- extendEnv -< ("c",3,env2)
          env4 <- extendEnv -< ("d",4,env3)
          env5 <- extendEnv -< ("e",5,env4)
          env6 <- extendEnv -< ("f",6,env5)
          env7 <- extendEnv -< ("g",7,env6)
          localEnv f -< (env7,x)
  
    it "env(a) = [1,6]" $ runTests setup "a" `shouldBe` Just (Interval 1 6)
    it "env(b) = [2,6]" $ runTests setup "b" `shouldBe` Just (Interval 2 7)
    it "env(c) = [3,3]" $ runTests setup "c" `shouldBe` Just (Interval 3 3)
    it "env(d) = [4,4]" $ runTests setup "d" `shouldBe` Just (Interval 4 4)
    it "env(e) = [5,5]" $ runTests setup "e" `shouldBe` Just (Interval 5 5)
    it "env(f) = [1,6]" $ runTests setup "f" `shouldBe` Just (Interval 1 6)
    it "env(g) = [2,7]" $ runTests setup "g" `shouldBe` Just (Interval 2 7)

  context "env = [a -> 1, d -> 4, g -> 7] with allocation strategy (addr+1)%5" $ do
    let setup :: Ar x y -> Ar x y
        setup f = proc x -> do
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
          localEnv f -< (env7,x)
  
    it "env(a) = [1,6]" $ runTests setup "a" `shouldBe` Just (Interval 1 6)
    it "env(b) = Nothing" $ runTests setup "b" `shouldBe` Nothing
    it "env(c) = Nothing" $ runTests setup "c" `shouldBe` Nothing
    it "env(d) = [4,4]" $ runTests setup "d" `shouldBe` Just (Interval 4 4)
    it "env(e) = Nothing" $ runTests setup "e" `shouldBe` Nothing
    it "env(f) = Nothing" $ runTests setup "f" `shouldBe` Nothing
    it "env(g) = [2,7]" $ runTests setup "g" `shouldBe` Just (Interval 2 7)

  where
    runTests s x = snd (runStateArrow (runBoundedEnv (s lookup)) (0,([],x)))
