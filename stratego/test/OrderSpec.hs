{-# LANGUAGE OverloadedStrings #-}
module OrderSpec(main, spec) where

import qualified Data.Abstract.Powerset as A
import qualified Data.Abstract.PreciseStore as S
import qualified Data.Concrete.Powerset as P
import           Data.Order
import qualified WildcardSemantics as W

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  
  it "ordering on abstract powersets is correct" $
    pow [Right (W.Cons "f" [W.Cons "g" [], W.Cons "h" []]), Left ()] ⊑ pow [Right W.Wildcard, Left ()]
         `shouldBe` True

  it "ordering of abstract powersets of results is correct" $ do
    pow [Left ()::Either () (W.Term,W.TermEnv)] ⊑ pow [Right (W.Wildcard,S.empty), Left ()]
         `shouldBe` True

    pow [Right (W.Cons "g" [],S.empty)::Either () (W.Term,W.TermEnv)] ⊑ pow [Right (W.Wildcard,S.empty), Left ()]
         `shouldBe` True

    pow [Right (W.Wildcard,S.empty)::Either () (W.Term,W.TermEnv)] ⊑ pow [Right (W.Cons "g" [],S.empty), Left ()]
         `shouldBe` False

  where
    pow :: Foldable f => f a -> A.Pow a
    pow = P.fromFoldable
