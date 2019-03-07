{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Case studies that run the wildcard semantics on given Stratego
-- programs.
module Main where

import           Syntax hiding (Fail)
import qualified WildcardSemantics as W
import qualified Data.Abstract.Powerset as W

import qualified Pretty.Haskell as H
import qualified Pretty.JavaScript as J
import qualified Pretty.PCF as P
import           Pretty.Results

import           Paths_sturdy_stratego

import qualified Criterion as CM
import qualified Criterion as CT

import           Data.ATerm
import           Data.Abstract.Error
import qualified Data.Abstract.Failure as F
import qualified Data.Abstract.Map as S
import           Data.Abstract.Terminating
import           Data.Foldable
import           Data.HashSet (HashSet)
import qualified Data.HashSet as H
import           Data.String
import qualified Data.Text.IO as TIO

import           Text.PrettyPrint
import           Text.Printf

-- | Runs the case studies.
main :: IO ()
main = do
    prettyPrint H.ppHaskell =<< caseStudy (W.eval 5) "arrows" "desugar_arrow_0_0"
    prettyPrint H.ppHaskell =<< caseStudy (W.eval 5) "cca" "norm_0_0"
    prettyPrint P.ppPCF     =<< caseStudy (W.eval 5) "arith" "eval_0_0"
    prettyPrint P.ppPCF     =<< caseStudy (W.eval 5) "pcf" "eval_0_0"
    prettyPrint P.ppPCF     =<< caseStudy (W.eval 5) "pcf" "check_eval_0_0"
    prettyPrint J.tryPPJS   =<< caseStudy (W.eval 5) "go2js" "generate_js_ast_0_0"

-- | Pretty prints a given set of abstract terms using the given
-- pretty printer.
prettyPrint :: (W.Term -> Doc) -> HashSet W.Term -> IO ()
prettyPrint pprint res =
  if H.size res <= 200
     then print $ ppResults pprint (toList res)
     else printf "Output ommited because the result set contains %d elements\n" (H.size res)

-- | Runs the given semantics on the given function within the given
-- case study.
caseStudy :: (Strat -> StratEnv -> W.TermEnv -> W.Term -> Terminating (W.Pow (F.Failure String (Error () (W.TermEnv,W.Term))))) -> String -> String -> IO (HashSet W.Term)
caseStudy eval name function = do
  printf "------------------ case study: %s ----------------------\n" name
  file <- TIO.readFile =<< getDataFileName (printf "case-studies/%s/%s.aterm" name name)
  case parseModule =<< parseATerm file of
    Left e -> fail (show e)
    Right module_ -> do
      let res = fromTerminating (error "non-terminating wildcard semantics") $ eval (Call (fromString function) [] []) (stratEnv module_) S.empty W.Wildcard
      let terms = H.fromList $ toList $ filterResults (toList res)

      CT.benchmark (CT.nfIO (return terms))
      return terms
 where
   filterResults = fmap (\r -> case r of Success (_,t) -> t; SuccessOrFail _ (_,t) -> t; Fail _ -> error "")
                 . filter (\r -> case r of Success _ -> True; SuccessOrFail _ _ -> True; Fail _ -> False)
                 . fmap (\r -> case r of F.Success t -> t; F.Fail msg -> error msg)
