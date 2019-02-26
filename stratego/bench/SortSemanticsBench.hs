{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
module Main where

import           Prelude hiding (exp)

import           SortSemantics -- hiding (sortContext)
import           Syntax hiding (Fail)
import qualified CaseStudy

import           SortContext(Context,Sort(..))
import qualified SortContext as Ctx

import qualified Data.Abstract.WeakMap as S
import qualified Data.HashMap.Lazy as M


import           Criterion
import           Criterion.Main

main :: IO ()
main = do
  pcf <- CaseStudy.pcf
  defaultMain [
      bgroup "Sort Semantics" [
        bench "reduce Add(Zero,y)" $
          let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp"),("Add",["Exp","Exp"],"Exp")] in
          let exp = term "Exp"
              prog = Match (Cons "Add" [Cons "Zero" [], "y"]) `Seq` Build "y"
          in nf (eval 0 10 prog M.empty ?ctx emptyEnv) exp
        ,
        bench "should support recursion" $
          let ?ctx = Ctx.empty in
          let t = convertToList [numerical, numerical, numerical] ?ctx
              prog = Let [("map", map')] (Scope ["x"] (Match "x" `Seq` Call "map" [Build (NumberLiteral 1)] ["x"]))
          in nf (eval 2 3 prog M.empty ?ctx emptyEnv) t
        ,
        bench "pcf" $
          let ?ctx = signature pcf in
          let senv = stratEnv pcf
          in nf (eval 5 10 (Call "eval_0_0" [] []) senv ?ctx emptyEnv) (term (Tuple [List (Tuple [Lexical, "Val"]), "Exp"]))
      ]
    ]
  where
    term :: (?ctx :: Context) => Sort -> Term
    term s = Term s ?ctx

    numerical :: (?ctx :: Context) => Term
    numerical = term Numerical

    emptyEnv :: TermEnv
    emptyEnv = S.empty

    map' = Strategy ["f"] ["l"] (Scope ["x","xs","x'","xs'"] (
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
