-- {-# LANGUAGE OverloadedStrings #-}
module SignAnalysisSpec where

import           SharedSpecs

-- import           Data.Error
-- import qualified Data.HashMap.Lazy as M
-- import qualified Data.Powerset as P
-- import           Data.Sign
-- import qualified Data.Text as T
-- import qualified PCF as E
import           SignAnalysis hiding (Top)

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  sharedSpec evalSign (NumVal . fromIntegral)
