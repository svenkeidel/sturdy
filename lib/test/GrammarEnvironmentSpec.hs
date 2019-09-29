{-# LANGUAGE OverloadedLists #-}
module GrammarEnvironmentSpec where

import qualified Data.Abstract.Environment.Grammar as Env
import           Data.Concrete.Closure

import           Test.Hspec

type Env = Env.Env String (Closure Int)

spec :: Spec
spec = do
  it "Env.fromList should create a mutual recursive environment" $ do
    let env = Env.fromList [ ("foo", Closure 1 ())
                           , ("bar", Closure 2 ())] :: Env
    Env.lookup "foo" env `shouldBe` Just (Closure 1 env)
    Env.lookup "bar" env `shouldBe` Just (Closure 2 env)

  it "inserting closures into empty environment should not create a recursive binding" $ do
    let env1 = Env.insert "foo" (Closure 1) Env.empty :: Env
        env2 = Env.insert "bar" (Closure 2) env1
    Env.lookup "foo" env2 `shouldBe` Just (Closure 1 Env.empty)
    Env.lookup "bar" env2 `shouldBe` Just (Closure 2 env1)

  it "inserting a recursive closure into an environment should not change the original bindings" $ do
    let env1 = Env.fromList [ ("foo", Closure 1 ())
                            , ("bar", Closure 2 ())] :: Env
        env2 = Env.insertRec "baz" (Closure 3) env1
    Env.lookup "foo" env2 `shouldBe` Just (Closure 1 env1)
    Env.lookup "bar" env2 `shouldBe` Just (Closure 2 env1)
    Env.lookup "baz" env2 `shouldBe` Just (Closure 3 env2)

  it "inserting a binding into the environment twice should keep the environment the same" $ do
    let env1 = Env.fromList [ ("foo", Closure 1 ())
                            , ("bar", Closure 2 ())] :: Env
        env2 = Env.insertRec "biz" (Closure 4)
             $ Env.insertRec "baz" (Closure 3) env1
        env3 = Env.insertRec "biz" (Closure 4)
             $ Env.insertRec "baz" (Closure 3) env2
    env2 `shouldBe` env3

  it "union of environments should be left biased" $ do
    let env1 = Env.fromList [ ("foo", Closure 1 ())
                            , ("bar", Closure 2 ())] :: Env
        env2 = Env.fromList [ ("foo", Closure 3 ())
                            , ("baz", Closure 4 ())] :: Env
        env = Env.union env1 env2

    Env.lookup "foo" env `shouldBe` Just (Closure 1 env1)
    Env.lookup "bar" env `shouldBe` Just (Closure 2 env1)
    Env.lookup "baz" env `shouldBe` Just (Closure 4 env2)
