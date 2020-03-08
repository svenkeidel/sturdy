{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC
  -fspecialise-aggressively
  -flate-specialise
  -fsimpl-tick-factor=500
#-}
module Main where

import           Criterion
import           Criterion.Main

import           Data.Label

import           System.Process(readCreateProcess,shell) -- deprecated
import           System.Directory(getCurrentDirectory)

import           Syntax (Expr,let_rec)
import           LispParser(readExprList)
import           LispToHask(match,getTopDefinesLam,getBody)
import           TypedAnalysis.Chaotic
import           TypedAnalysis.Parallel


main :: IO ()
main = defaultMain
    [
      bgroup "Gabriel" [
        -- benchAlgos "boyer" "gabriel//boyer",
        -- benchAlgos "cpstak" "gabriel//cpstak",
        -- benchAlgos "dderiv" "gabriel//dderiv",
        -- benchAlgos "deriv" "gabriel//deriv",
        benchAlgos "diviter" "gabriel//diviter",
        benchAlgos "divrec" "gabriel//divrec",
        benchAlgos "takl" "gabriel//takl"
      ],
      bgroup "Scala-AM" [
        benchAlgos "collatz" "scala-am//collatz",
        benchAlgos "gcipd" "scala-am//gcipd",
        benchAlgos "nqueens" "scala-am//nqueens"
        -- benchAlgos "primtest" "scala-am//primtest",
        -- benchAlgos "rsa" "scala-am//rsa"
      ]
    ]
  where
    benchAlgos name file =
      env (loadSchemeFile file) $ \expr ->
        let ?sensitivity = 0 in bgroup name [
          bench "chaotic.innermost" $ nf evalInnermost expr,
          bench "chaotic.outermost" $ nf evalOutermost expr,
          bench "parallel.stack" $ nf evalParallel expr,
          bench "parallel.ADI" $ nf evalParallelADI expr
        ]

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
  readCreateProcess (shell $ "/nix/store/5yw6qgbbmjyqymzqckzzkyhk8c41s796-racket-7.4/bin/raco expand " ++ root') ""
