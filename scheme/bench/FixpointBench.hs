{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=200 #-}
module Main where

import           Control.Monad.State(State)

import           Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Chaotic(innermost)
import qualified Control.Arrow.Fix.Context as Ctx
import           Control.Arrow.Trans
import           Control.Arrow.Environment

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
import           Data.Abstract.Terminating

import           System.Process(readCreateProcess,shell) -- deprecated
import           System.Directory(getCurrentDirectory)

import           Syntax (Expr(App),let_rec,apply)
import           LispParser(readExprList)
import           LispToHask(match,getTopDefinesLam,getBody)
import           TypedAnalysis(Val,Store,Env,Addr,Ctx)
import           GenericInterpreter as Generic


main :: IO ()
main = do
  defaultMain
    [
      bgroup "Scala-AM" [
        benchAlgos "collatz" "scala-am//collatz"
      ]
    ]
  where
    benchAlgos name file =
      env (loadSchemeFile file) $ \expr ->
        bgroup name [
          let ?sensitivity = 0 in
          bench "chaotic.innermost" $ nf evalInnermost expr
        ]

type Interp x y =
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

type In = (Store,(([Expr],Label),Env))
type Out = (Store, (HashSet Text, Terminating Val))


{-# SPECIALIZE Generic.run_ :: Interp [Expr] Val #-}
{-# SPECIALIZE Generic.eval :: Interp [Expr] Val -> Interp Expr Val #-}

evalInnermost :: (?sensitivity :: Int) => Expr -> Out
evalInnermost e0 = snd $ run (Generic.run_ :: Interp [Expr] Val)
    W.finite
    iterationStrategy
    (W.finite, W.finite)
    (empty,(empty,[e0]))
  where
    iterationStrategy =
      Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of [App _ _ l] -> Just l; _ -> Nothing) .
      Fix.filter apply innermost

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
  readCreateProcess (shell $ "/nix/store/j9cd9fp6jqrlqyr1vg8gwabi0azza3y7-user-environment/bin/raco expand " ++ root') ""
