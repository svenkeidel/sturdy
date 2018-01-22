{-# LANGUAGE OverloadedStrings #-}
module ConcreteSpec where

import           Prelude hiding (map,succ)

import qualified Data.Map as M
import           Shared
import           Concrete

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "evalConcrete" $ do
  let map = (fix (Lam "map" (Lam "f" (Lam "l" (Case (Var "l")
                            [(ConP "Nil" [], list []),
                             (ConP "Cons" ["x","xs"], Con "Cons" [App "f" "x", App (App "map" "f") "xs"])])))))

  it "'map (+1)' should increase the number of each element of a list" $
    evalConcrete M.empty (App (App map plusOne) (list [one,three,two]))
      `shouldBe` Right (litV (list [two,four,three]))

   where
     -- Call-by-value fixpoint combinator
     fix = App (Lam "f" (App (Lam "x" (App "f" (Lam "v" (App (App "x" "x") "v")))) (Lam "x" (App "f" (Lam "v" (App (App "x" "x") "v"))))))
     zero = Con "Z" []
     succ x = Con "S" [x]
     one = succ zero
     two = succ one
     three = succ two
     four = succ three
     plusOne = Lam "x" (Con "S" ["x"])
     list [] = Con "Nil" []
     list (x:xs) = Con "Cons" [x,list xs]
     litV (Con c ts) = ConV c (fmap litV ts)
     litV _ = error "can only convert literals"
