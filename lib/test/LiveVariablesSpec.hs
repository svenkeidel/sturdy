{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module LiveVariablesSpec where

import           Prelude hiding (read,sequenceA,Bounded)

import           Control.Arrow.Store
import           Control.Arrow.Fix
import           Control.Arrow.Fail
import           Control.Arrow.Transformer.Abstract.LiveVariables
import qualified Control.Arrow.Transformer.Abstract.LiveVariables as L
import           Control.Arrow.Transformer.Abstract.Store
import           Control.Arrow.Transformer.Abstract.Except
import           Control.Arrow.Transformer.Abstract.Fix

import           Data.Ord (comparing)
import           Data.List
import           Data.Text (Text)

import           Data.Abstract.Interval
import qualified Data.Abstract.Store as S
import           Data.Abstract.InfiniteNumbers
import           Data.Abstract.Bounded

import           Test.Hspec

main :: IO ()
main = hspec spec

type IV = Bounded (Interval (InfiniteNumber Int))
type Arr = Fix [Int] () (LiveVariables Text (StoreArrow Text IV (Except String (~>))))

spec :: Spec
spec = do
  it "x:=1" $ do
    runArr' L.empty [proc () -> write -< ("x",num 1 1)]
      `shouldBe` [(0,[])]

  it "x:=1; y:=x" $ do
    runArr' L.empty [proc () -> write -< ("x",num 1 1), proc () -> do x <- read -< "x"; write -< ("y",x)]
      `shouldBe` [(0,["x"]),(1,[])]

  where
    num i j = Bounded (Interval (-100) 100) (Interval i j)

runArr :: Arr () () -> LiveVars Text -> [(Int,LiveVars Text)]
runArr f vs = sortBy (comparing fst) $ fmap ((\(l,is) -> (head is,l)) . snd . fst) 
  $ S.toList $ fst $ runFix' (runExcept (runStore (runLiveVariables f))) (S.empty,(vs,()))

sequenceA :: [Arr () ()] -> Arr () ()
sequenceA l = proc () ->
  (fixA $ \f -> proc is -> case is of
    [i] -> do
      l !! i -<< ()
    (i:xs) -> do
      l !! i -<< ()
      f -< xs
    [] -> failA -< "cannot sequence empty list")
    -< [ i | (_,i) <- zip l [0..]]

runArr' :: LiveVars Text -> [Arr () ()] -> [(Int,LiveVars Text)]
runArr' vs ss = runArr (sequenceA ss) vs
  
