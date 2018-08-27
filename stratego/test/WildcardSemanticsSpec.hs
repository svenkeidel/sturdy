{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module WildcardSemanticsSpec(main, spec) where

import           Prelude hiding (map)
    
import qualified ConcreteSemantics as C
import           SharedSemantics hiding (cons)
import           Soundness
import           Syntax hiding (Fail)
import qualified WildcardSemantics as W

import           Control.Arrow

import           Data.Abstract.HandleError
import qualified Data.Abstract.Powerset as A
import qualified Data.Abstract.PreciseStore as S
import qualified Data.Concrete.Powerset as C
import           Data.GaloisConnection
import qualified Data.HashMap.Lazy as M
import           Data.Order
import           Data.Term (TermUtils(..))
    
import           Text.Printf

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (Success)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "let" $
    it "should work for the abstract case" $ do
      let cons x xs = W.Cons "Cons" [x,xs]
      let t = cons 2 W.Wildcard
      fmap snd <$> weval 2 (Let [("map", map)]
                  (Match "x" `Seq`
                   Call "map" [Build 1] ["x"])) t
        `shouldBe'`
           C.fromFoldable
             [ Success $ convertToList [1]
             , Success $ convertToList [1,1]
             , Success $ convertToList [1,1,1]
             , Fail ()
             , Fail ()
             , Success (cons 1 (cons 1 (cons 1 (cons W.Wildcard W.Wildcard))))]

  describe "call" $
    prop "should be sound" $ do
      i <- choose (0,10)
      j <- choose (0,10)
      l <- C.similarTerms i 7 2 10
      let (l1,l2) = splitAt j l
      let t1 = convertToList l1
      let t2 = convertToList l2
      return $ counterexample (printf "t: %s\n"
                                      (showLub t1 t2))
             $ sound' (Let [("map", map)]
                  (Match "x" `Seq`
                   Call "map" [Build 1] ["x"]))
                  [(t1,[]),(t2,[])]

  describe "match" $ do

    prop "should handle inconsistent environments" $ do
      let t1 = C.Cons "f" []
          t2 = C.Cons "g" []
      sound' (Match "x") [(t1, [("x", t1)]), (t2, [("y", t2)])]

    prop "should be sound" $ do
      [t1,t2,t3] <- C.similarTerms 3 7 2 10
      matchPattern <- C.similarTermPattern t1 3
      return $ counterexample
                 (printf "pattern: %s\n %s ⊔ %s = %s"
                    (show matchPattern) (show t2) (show t3)
                    (showLub t2 t3))
             $ sound' (Match matchPattern) [(t2,[]),(t3,[])]

  describe "build" $
    prop "should be sound" $ do
      [t1,t2,t3] <- C.similarTerms 3 7 2 10
      matchPattern <- C.similarTermPattern t1 3
      let vars = patternVars' matchPattern
      buildPattern <- arbitraryTermPattern 5 2 $
        if not (null vars) then elements vars else arbitrary
      return $ counterexample
                 (printf "match pattern: %s\nbuild pattern: %s\nt2: %s\nt3: %s\nlub t2 t3 = %s"
                    (show matchPattern) (show buildPattern) (show t2) (show t3)
                    (showLub t2 t3))
             $ sound' (Match matchPattern `Seq` Build buildPattern) [(t2,[]),(t3,[])]

  -- describe "lookupTermVar" $
  --   prop "should be sound" $ do
  --     sound M.empty lookupTermVar lookupTermVar

  where
    sound' :: Strat -> [(C.Term,[(TermVar,C.Term)])] -> Property
    sound' s xs = sound M.empty (C.fromFoldable $ fmap (second termEnv) xs) (eval' s) (eval' s :: W.Interp W.Term W.Term)

    termEnv = C.TermEnv . M.fromList

    showLub :: C.Term -> C.Term -> String
    showLub t1 t2 = show (alpha (C.fromFoldable [t1,t2] :: C.Pow C.Term) :: W.Term) 

    shouldBe' :: A.Pow (Error () W.Term) -> A.Pow (Error () W.Term) -> Property
    shouldBe' s1 s2 = counterexample (printf "%s < %s\n" (show s1) (show s2)) (s2 ⊑ s1 `shouldBe` True)
    infix 1 `shouldBe'`

    map = Strategy ["f"] ["l"] (Scope ["x","xs","x'","xs'"] (
            Build "l" `Seq`
            GuardedChoice
              (Match (Cons "Cons" ["x","xs"]))
              (Build "x" `Seq`
               Call "f" [] [] `Seq`
               Match "x'" `Seq`
               Call "map" ["f"] ["xs"] `Seq`
               Match "xs'" `Seq`
               Build (Cons "Cons" ["x'", "xs'"]))
              (Build (Cons "Nil" []))))

    weval :: Int -> Strat -> W.Term -> A.Pow (Error () (W.TermEnv,W.Term))
    weval i s = W.eval i s M.empty S.empty
