-- {-# LANGUAGE OverloadedStrings #-}
module ConcreteSpec where

import           SharedSpecs
import           Concrete
-- import           Data.Error
-- import qualified Data.HashMap.Lazy as M
-- import qualified Data.Text as T
-- import qualified PCF as E
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  sharedSpec evalConcrete (NumVal . fromIntegral)
