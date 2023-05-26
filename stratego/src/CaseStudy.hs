{-# LANGUAGE FlexibleContexts #-}
module CaseStudy where

import           Syntax (LStratEnv,parseModule,signature,moduleStratEnv)
import qualified Data.Text.IO as TIO
import qualified Data.ATerm as A
import           SortContext

import           Control.Monad.Except
import           Control.Monad.State

import           Paths_sturdy_stratego
import           Text.Printf

pcf :: IO (Context,LStratEnv)
pcf = caseStudy "pcf"

nnf :: IO (Context,LStratEnv)
nnf = caseStudy "nnf"

cca :: IO (Context,LStratEnv)
cca = caseStudy "cca"

arrows :: IO (Context,LStratEnv)
arrows = caseStudy "arrows"

balg :: IO (Context,LStratEnv)
balg = caseStudy "balg"

caseStudy :: String -> IO (Context,LStratEnv)
caseStudy name = do
  file <- TIO.readFile =<< getDataFileName (printf "case-studies/%s/%s.aterm" name name)
  case runExcept (runStateT (parseModule =<< A.parseATerm file) 0) of
    Left e -> fail (printf "parsing of case study %s failed: %s" name (show e))
    Right (module_,g) -> return (signature module_, do put g; return (moduleStratEnv module_))

