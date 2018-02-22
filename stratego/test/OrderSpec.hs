{-# LANGUAGE OverloadedStrings #-}
module OrderSpec(main, spec) where

import qualified Data.AbstractPowerset as A
import qualified Data.HashMap.Lazy as M
import           Data.Order
import qualified Data.Powerset as P
import qualified WildcardSemantics as A

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  
  it "ordering on abstract powersets is correct" $
    ( pow [Right (A.Cons "f" [A.Cons "g" [], A.Cons "h" []]), Left ()] ⊑ pow [Right A.Wildcard, Left ()] )
         `shouldBe` True

  it "ordering on environments" $
    termEnv [("x",A.Cons "f" []), ("y",A.Cons "g" [])] ⊑ termEnv [("x",A.Wildcard)] 
         `shouldBe` True

  it "ordering of abstract powersets of results is correct" $ do
    pow [Left ()] ⊑ pow [Right (A.Wildcard,termEnv []), Left ()]
         `shouldBe` True

    pow [Right (A.Cons "g" [], termEnv [])] ⊑ pow [Right (A.Wildcard,termEnv []), Left ()]
         `shouldBe` True

    pow [Right (A.Wildcard, termEnv [])] ⊑ pow [Right (A.Cons "g" [],termEnv []), Left ()]
         `shouldBe` False

  where
    termEnv = A.TermEnv . M.fromList
    pow :: Foldable f => f a -> A.Pow a
    pow = P.fromFoldable

