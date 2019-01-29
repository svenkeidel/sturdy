{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
module Main where

import           Prelude hiding (exp)

import           SortSemantics -- hiding (sortContext)
import           Syntax hiding (Fail)

import           SortContext(Context,Sort(..))
import qualified SortContext as Ctx

import qualified Data.Abstract.Map as S
import qualified Data.HashMap.Lazy as M

import           Criterion
import           Criterion.Main

main :: IO ()
main = defaultMain [
    bgroup "Least Fixpoint" [
      bench "reduce Add(Zero,y)" $
        let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp"),("Add",["Exp","Exp"],"Exp")] in
        let exp = term "Exp"
            reduce = Match (Cons "Add" [Cons "Zero" [], "y"]) `Seq` Build "y"
        in nf (eval 0 10 reduce M.empty ?ctx emptyEnv) exp
    ],

    bgroup "Greatest Fixpoint" [
      bench "reduce Add(Zero,y)" $
        let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp"),("Add",["Exp","Exp"],"Exp")] in
        let exp = term "Exp"
            reduce = Match (Cons "Add" [Cons "Zero" [], "y"]) `Seq` Build "y"
        in nf (eval' 10 reduce M.empty ?ctx emptyEnv) exp
    ]
  ]
  where
    term :: (?ctx :: Context) => Sort -> Term
    term s = Term s ?ctx

    termEnv :: [(TermVar, Term)] -> TermEnv
    termEnv = S.fromList

    emptyEnv :: TermEnv
    emptyEnv = S.empty
