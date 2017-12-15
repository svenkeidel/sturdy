{-# LANGUAGE OverloadedStrings #-}
module Combine where

import Data.ATerm
import qualified Data.Text.IO as T
import System.Environment

main :: IO ()
main = do
  [ast,core] <- getArgs
  astATerm <- parseATerm <$> T.readFile ast
  coreATerm <- parseATerm <$> T.readFile core
  case (astATerm,coreATerm) of
    (Right (ATerm "Specification" [List (sig:_)]),
     Right (ATerm "Specification" [List [_, rules]])) ->
      print (ATerm "Specification" [List [sig,rules]])
    (Left _, Right _) -> error "Ast file did not parse correctly"
    (Right _, Left _) -> error "Core file did not parse correctly"
    (Left _, Left _) -> error "Ast and core file did not parse correctly"
    (_, _) -> error "Unexpected ATerm"
