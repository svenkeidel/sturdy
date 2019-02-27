module CaseStudy where

import           Syntax
import qualified Data.Text.IO as TIO
import qualified Data.ATerm as A

import           Paths_sturdy_stratego
import           Text.Printf

pcf :: IO Module
pcf = caseStudy "pcf"

nnf :: IO Module
nnf = caseStudy "nnf"

arrows :: IO Module
arrows = caseStudy "arrows"

arrDesugarCaseStudy :: IO Module
arrDesugarCaseStudy = caseStudy "arrows"

caseStudy :: String -> IO Module
caseStudy name = do
  file <- TIO.readFile =<< getDataFileName (printf "case-studies/%s/%s.aterm" name name)
  case parseModule =<< A.parseATerm file of
    Left e -> fail (show e)
    Right module_ -> return module_

