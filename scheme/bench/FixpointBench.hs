{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=200 #-}
module Main where

import           Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Chaotic(innermost,outermost)
import qualified Control.Arrow.Fix.Context as Ctx
import           Control.Arrow.Fix.Parallel
import           Control.Arrow.Trans

import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.FiniteEnvStore
import           Control.Arrow.Transformer.Abstract.LogError
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Component
import           Control.Arrow.Transformer.Abstract.Fix.Context
import           Control.Arrow.Transformer.Abstract.Fix.Stack
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable(CacheT,Monotone)
import           Control.Arrow.Transformer.Abstract.Terminating

import           Criterion
import           Criterion.Main

import           Data.HashSet(HashSet)
import           Data.Text(Text)
import           Data.Label
import           Data.Empty

import qualified Data.Abstract.Widening as W

import           System.Process(readCreateProcess,shell) -- deprecated
import           System.Directory(getCurrentDirectory)

import           Syntax (Expr(App),let_rec,apply)
import           LispParser(readExprList)
import           LispToHask(match,getTopDefinesLam,getBody)
import           TypedAnalysis(Val,Addr,Ctx,In,Out)
import           GenericInterpreter as Generic


main :: IO ()
main = defaultMain
    [
      bgroup "Gabriel" [
        -- benchAlgos "boyer" "gabriel//boyer",
        -- benchAlgos "cpstak" "gabriel//cpstak",
        -- benchAlgos "dderiv" "gabriel//dderiv",
        -- benchAlgos "deriv" "gabriel//deriv",
        -- benchAlgos "diviter" "gabriel//diviter",
        benchAlgos "divrec" "gabriel//divrec",
        benchAlgos "takl" "gabriel//takl"
      ],
      bgroup "Scala-AM" [
        benchAlgos "collatz" "scala-am//collatz",
        benchAlgos "gcipd" "scala-am//gcipd"
        -- benchAlgos "nqueens" "scala-am//nqueens"
        -- benchAlgos "primtest" "scala-am//primtest",
        -- benchAlgos "rsa" "scala-am//rsa"
      ]
    ]
  where
    benchAlgos name file =
      env (loadSchemeFile file) $ \expr ->
        let ?sensitivity = 0 in bgroup name [
          bench "chaotic.innermost" $ nf evalInnermost expr
          -- bench "chaotic.outermost" $ nf evalOutermost expr,
          -- bench "parallel.stack" $ nf evalOutermost expr,
          -- bench "parallel.ADI" $ nf evalOutermost expr
        ]

type InterpChaotic x y =
  Fix
    (ValueT Val
      (TerminatingT
        (LogErrorT (HashSet Text)
          (EnvStoreT Text Addr Val
            (FixT () ()
              (ComponentT In
                (StackT Stack In
                  (CacheT Monotone In Out
                    (ContextT Ctx
                      (->)))))))))) [Expr] Val x y

{-# SPECIALIZE Generic.run_ :: InterpChaotic [Expr] Val #-}
{-# SPECIALIZE Generic.eval :: InterpChaotic [Expr] Val -> InterpChaotic Expr Val #-}

type InterpParallel x y =
  Fix
    (ValueT Val
      (TerminatingT
        (LogErrorT (HashSet Text)
          (EnvStoreT Text Addr Val
            (FixT () ()
              (StackT Stack In
                (CacheT Monotone In Out
                  (ContextT Ctx
                    (->))))))))) [Expr] Val x y

{-# SPECIALIZE Generic.run_ :: InterpParallel [Expr] Val #-}
{-# SPECIALIZE Generic.eval :: InterpParallel [Expr] Val -> InterpParallel Expr Val #-}

evalInnermost :: (?sensitivity :: Int) => Expr -> Out
evalInnermost e0 = snd $ run (Generic.run_ :: InterpChaotic [Expr] Val)
    W.finite
    iterationStrategy
    (W.finite, W.finite)
    (empty,(empty,[e0]))
  where
    iterationStrategy =
      Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of [App _ _ l] -> Just l; _ -> Nothing) .
      Fix.filter apply innermost

evalOutermost :: (?sensitivity :: Int) => Expr -> Out
evalOutermost e0 = snd $ run (Generic.run_ :: InterpChaotic [Expr] Val)
    W.finite
    iterationStrategy
    (W.finite, W.finite)
    (empty,(empty,[e0]))
  where
    iterationStrategy =
      Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of [App _ _ l] -> Just l; _ -> Nothing) .
      Fix.filter apply outermost

evalParallel :: (?sensitivity :: Int) => Expr -> Out
evalParallel e0 = snd $ run (Generic.run_ :: InterpParallel [Expr] Val)
    W.finite
    iterationStrategy
    (W.finite, W.finite)
    (empty,(empty,[e0]))
  where
    iterationStrategy =
      Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of [App _ _ l] -> Just l; _ -> Nothing) .
      Fix.filter apply parallel

evalParallelADI :: (?sensitivity :: Int) => Expr -> Out
evalParallelADI e0 = snd $ run (Generic.run_ :: InterpParallel [Expr] Val)
    W.finite
    iterationStrategy
    (W.finite, W.finite)
    (empty,(empty,[e0]))
  where
    iterationStrategy =
      Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of [App _ _ l] -> Just l; _ -> Nothing) .
      Fix.filter apply parallelADI

loadSchemeFile :: String -> IO Expr
loadSchemeFile inFile = do
  file_str <- helper_import inFile
  case readExprList file_str of
    Right a -> case match a of
      Right lisp -> return $ generate $ let_rec (getTopDefinesLam lisp) (getBody lisp)
      Left err  -> fail err
    Left err -> fail (show err)

helper_import :: String -> IO String
helper_import inFile = do
  root <- getCurrentDirectory
  let root' = root ++ "//scheme_files//" ++  inFile ++ ".scm"
  readCreateProcess (shell $ "raco expand " ++ root') ""
