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
        benchAlgos "boyer" "gabriel/boyer.scm",
        benchAlgos "browse" "gabriel/browse.scm",
        benchAlgos "cpstak" "gabriel/cpstak.scm",
        benchAlgos "dderiv" "gabriel/dderiv.scm",
        benchAlgos "deriv" "gabriel/deriv.scm",
        benchAlgos "diviter" "gabriel/diviter.scm",
        benchAlgos "destruc" "gabriel/destruc.scm",
        benchAlgos "divrec" "gabriel/divrec.scm",
        benchAlgos "takl" "gabriel/takl.scm"
      ],
      bgroup "Scala-AM" [
        benchAlgos "collatz" "scala-am/collatz.scm",
        benchAlgos "gcipd" "scala-am/gcipd.scm",
        benchAlgos "nqueens" "scala-am/nqueens.scm",
        benchAlgos "primtest" "scala-am/primtest.scm",
        benchAlgos "rsa" "scala-am/rsa.scm"
      ]
    ]
  where
    benchAlgos name file =
      env (loadSchemeFile' file) $ \expr ->
        let ?sensitivity = 0 in
        bgroup name [
          bench "chaotic.innermost" $ nf evalInner expr,
          bench "chaotic.outermost" $ nf evalOuter expr,
          bench "parallel" $ nf evalParallel expr,
          bench "ADI" $ nf evalADI expr
        ]
