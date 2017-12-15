{-# LANGUAGE OverloadedStrings #-}
module OrderSpec(main, spec) where

import qualified WildcardSemantics as A
import           Data.Order
import           Data.Result
import qualified Data.HashMap.Lazy as M
import qualified Data.Powerset as P
import qualified Data.AbstractPowerset as A

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (Result(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  
  it "ordering on abstract powersets is correct" $
    ( pow [Success (A.Cons "f" [A.Cons "g" [], A.Cons "h" []]), Fail] ⊑ pow [Success A.Wildcard, Fail] )
         `shouldBe` True

  it "ordering on environments" $
    termEnv [("x",A.Cons "f" []), ("y",A.Cons "g" [])] ⊑ termEnv [("x",A.Wildcard)] 
         `shouldBe` True

  it "ordering of abstract powersets of results is correct" $ do
    pow [Fail] ⊑ pow [Success (A.Wildcard,termEnv []), Fail]
         `shouldBe` True

    pow [Success (A.Cons "g" [], termEnv [])] ⊑ pow [Success (A.Wildcard,termEnv []), Fail]
         `shouldBe` True

    pow [Success (A.Wildcard, termEnv [])] ⊑ pow [Success (A.Cons "g" [],termEnv []), Fail]
         `shouldBe` False

  where
    termEnv = A.TermEnv . M.fromList
    pow :: Foldable f => f a -> A.Pow a
    pow = P.fromFoldable

