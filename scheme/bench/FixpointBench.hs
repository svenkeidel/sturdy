{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC
  -fspecialise-aggressively
  -flate-specialise
  -fsimpl-tick-factor=500
#-}
module Main where

import           Criterion
import           Criterion.Main

import           Parser(loadSchemeFile')
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
        benchAlgos "diviter" "gabriel//diviter.scm",
        benchAlgos "divrec" "gabriel//divrec.scm",
        benchAlgos "takl" "gabriel//takl.scm"
      ],
      bgroup "Scala-AM" [
        benchAlgos "collatz" "scala-am//collatz.scm",
        benchAlgos "gcipd" "scala-am//gcipd.scm",
        benchAlgos "nqueens" "scala-am//nqueens.scm"
        -- benchAlgos "primtest" "scala-am//primtest",
        -- benchAlgos "rsa" "scala-am//rsa"
      ]
    ]
  where
    benchAlgos name file =
      env (loadSchemeFile' file) $ \expr ->
        let ?sensitivity = 0 in
        bgroup name [
          bench "chaotic.innermost" $ nf evalInner expr,
          bench "chaotic.outermost" $ nf evalOuter expr,
          bench "parallel" $ nf evalPar expr,
          bench "ADI" $ nf evalADI expr
        ]
