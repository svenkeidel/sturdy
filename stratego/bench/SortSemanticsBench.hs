{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
module Main where

import           SortSemantics -- hiding (sortContext)
import           Syntax hiding (Fail)

import           SortContext(Context,Sort(..))
import qualified SortContext as Ctx

import qualified Data.Abstract.Map as S
import qualified Data.HashMap.Lazy as M
import           Data.Abstract.Failure(Failure)
import           Data.Abstract.Error(Error)
import           Data.Abstract.FreeCompletion(fromCompletion)
import           Data.Abstract.Terminating(fromTerminating)

import           Criterion
import           Criterion.Main

main :: IO ()
main = defaultMain [
     bench "build" $
      let ?ctx = Ctx.empty
      in nf (seval 0 (Build (StringLiteral "foo"))) bottom
  ]

  where
    term :: (?ctx :: Context) => Sort -> Term
    term s = Term s ?ctx

    termEnv :: [(TermVar, Term)] -> TermEnv
    termEnv = S.fromList

    emptyEnv :: TermEnv
    emptyEnv = S.empty


    seval :: Int -> Strat -> Term -> Failure String (Error () (TermEnv,Term))
    seval i s = seval'' i 10 s M.empty emptyEnv

    seval' :: Int -> Strat -> TermEnv -> Term -> Failure String (Error () (TermEnv,Term))
    seval' i s = seval'' i 10 s M.empty

    seval'' :: Int -> Int -> Strat -> StratEnv -> TermEnv -> Term -> Failure String (Error () (TermEnv,Term))
    seval'' i j s senv tenv t = fromCompletion (error "top element")
                               (fromTerminating (error "sort semantics does not terminate")
                                (eval i j s senv (context t) tenv t))

    bottom :: (?ctx :: Context) => Term
    bottom = term Bottom
