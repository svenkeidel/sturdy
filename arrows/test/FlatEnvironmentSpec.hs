{-# LANGUAGE OverloadedLists #-}
module FlatEnvironmentSpec where

import           Prelude hiding (Maybe(..))

import qualified Data.Abstract.Environment.Flat as Env
import           Data.Abstract.Closure
import           Data.Abstract.Maybe
import           Data.Order

import           Test.Hspec

type Env = Env.Env String (Closure Int)

spec :: Spec
spec = do
  it "Env.fromList should create a mutual recursive environment" $ do
    let env = Env.fromList [ ("foo", closure 1 ())
                           , ("bar", closure 2 ())] :: Env
    Env.lookup "foo" env `shouldBe` Just (closure 1 env)
    Env.lookup "bar" env `shouldBe` Just (closure 2 env)

  it "inserting closures into empty environment should not create a recursive binding" $ do
    let env1 = Env.insertNonRec "foo" (closure 1 ()) Env.empty :: Env
        env2 = Env.insertNonRec "bar" (closure 2 ()) env1
    Env.lookup "foo" env2 `shouldBe` Just (closure 1 (Env.mask [] env2))
    Env.lookup "bar" env2 `shouldBe` Just (closure 2 (Env.mask ["foo"] env2))

  it "inserting a recursive closure into an environment should not change the original bindings" $ do
    let env1 = Env.fromList [ ("foo", closure 1 ())
                            , ("bar", closure 2 ())] :: Env
        env2 = Env.insertRec [("baz", closure 3 ())] env1
    Env.lookup "foo" env2 `shouldBe` Just (closure 1 (Env.mask ["foo", "bar"] env2))
    Env.lookup "bar" env2 `shouldBe` Just (closure 2 (Env.mask ["foo", "bar"] env2))
    Env.lookup "baz" env2 `shouldBe` Just (closure 3 env2)

  it "inserting a binding into the environment twice should keep the environment the same" $ do
    let env1 = Env.fromList [ ("foo", closure 1 ())
                            , ("bar", closure 2 ())] :: Env
        env2 = Env.insertRec [("biz", closure 4 ())]
             $ Env.insertRec [("baz", closure 3 ())] env1
        env3 = Env.insertRec [("biz", closure 4 ())]
             $ Env.insertRec [("baz", closure 3 ())] env2
    env2 `shouldBe` env3

  it "least upper bound of environments should combine closures" $ do
    let env1 = Env.fromList [ ("foo", closure 1 ())
                            , ("bar", closure 2 ())] :: Env
        env2 = Env.fromList [ ("foo", closure 3 ())
                            , ("baz", closure 4 ()) ] :: Env
        env = env1 ⊔ env2

    Env.lookup "foo" env `shouldBe` Just [ (1,Env.mask ["foo", "bar"] env)
                                         , (3,Env.mask ["foo", "baz"] env)
                                         ]
    Env.lookup "bar" env `shouldBe` JustNothing (closure 2 (Env.mask ["foo", "bar"] env))
    Env.lookup "baz" env `shouldBe` JustNothing (closure 4 (Env.mask ["foo", "baz"] env))
