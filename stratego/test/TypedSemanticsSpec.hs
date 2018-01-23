{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module TypedSemanticsSpec where

-- import           Prelude hiding (succ,abs)

-- import           TypedSemantics
-- import           Sort
-- import           Signature (Signature)
-- import qualified Signature as S
-- import           InterpreterArrow
-- import           Paths_sturdy_stratego
-- import           Syntax(parseModule,Strat(..))

-- import           Data.ATerm(parseATerm)
-- import           Data.TypedResult
-- import qualified Data.Term as T
-- import           Data.Text (Text)
-- import           Data.TermEnv
-- import qualified Data.Text.IO as TIO
-- import qualified Data.HashMap.Lazy as M

import           Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec = return ()

  -- do
  
  -- let sig = S.empty & S.insertSubtype "String" "Expr"
  --                   & S.insertSubtype "INT" "Expr"
  --                   & S.insertType "Add" (S.Fun ["Expr", "Expr"] "Expr")

  -- it "lists should belong to the correct sort" $ do
  --   term' S.empty (T.Cons "Nil" []) `shouldBe` Success (nil (List Bottom))
  --   term' S.empty (T.Cons "Cons" [StringLiteral "foo", StringLiteral "bar"]) `shouldBe`
  --     TypeError "tail of the list is not of type list"

  --   term' S.empty (T.Cons "Cons" [StringLiteral "foo", nil (List Bottom)]) `shouldBe`
  --     Success (cons (StringLiteral "foo") (nil (List "String")) (List "String"))

  --   term' sig (T.Cons "Cons" [StringLiteral "foo", nil (List Bottom)]) `shouldBe`
  --     Success (cons (StringLiteral "foo") (nil (List "String")) (List "String"))

  --   let t = List (Coproduct "INT" "String")
  --     in term' sig (T.Cons "Cons" [NumberLiteral 1, cons (StringLiteral "foo") (nil (List "String")) (List "String")]) `shouldBe`
  --          Success (cons (NumberLiteral 1) (cons (StringLiteral "foo") (nil t) t) t)

  --   term' sig (T.Cons "Cons" [some (NumberLiteral 1) (Option "INT"),
  --                             cons (none (Option Bottom)) (nil (List (Option Bottom))) (List (Option Bottom))])
  --     `shouldBe` Success
  --       (cons (some (NumberLiteral 1) (Option "INT"))
  --             (cons (none (Option "INT")) (nil (List (Option "INT"))) (List (Option "INT"))) (List (Option "INT")))


  -- it "options should belong to the correct sort" $ do
  --   term' sig (T.Cons "None" []) `shouldBe` Success (none (Option Bottom))
  --   term' sig (T.Cons "Some" [StringLiteral "foo"]) `shouldBe`
  --     Success (Cons "Some" [StringLiteral "foo"] (Option "String"))
  
  -- it "constructors should belong to the correct sort" $ do
  --   term' sig (T.Cons "Add" [NumberLiteral 1, StringLiteral "foo"]) `shouldBe`
  --     Success (Cons "Add" [NumberLiteral 1, StringLiteral "foo"] "Expr")
  --   term' sig (T.Cons "Add" [nil (List Bottom), StringLiteral "foo"]) `shouldBe`
  --     TypeError "constructor application not well typed: Add\nexpected arguments: [Expr,Expr]\nbut got: [List (Bottom),String]"

  -- it "tuples should be well sorted" $ do
  --   term' sig (T.Cons "" [NumberLiteral 1, StringLiteral "foo"]) `shouldBe`
  --     Success (Cons "" [NumberLiteral 1, StringLiteral "foo"] (Tuple ["INT","String"]))

  --   term' sig (T.Cons "" []) `shouldBe` Success (Cons "" [] (Tuple []))
 
  -- describe "Case Studies" $ describe "PCF" $ beforeAll parsePCFCaseStudy $
  --   it "should execute well typed programs without a problem" $ \m -> do
  --     evalPCF m (tup [nil (List Bottom), zero])
  --       `shouldBe` Success (zero, tenv)

  --     evalPCF m (tup [nil (List Bottom), abs "x" num (var "x")])
  --       `shouldBe` Success (abs "x" num (var "x"), tenv)
 
  --     evalPCF m (tup [nil (List Bottom), app (abs "x" num (succ (var "x"))) zero])
  --       `shouldBe` Success (succ zero, tenv)

  -- where

  --   term' :: Signature -> T.TermF Term -> TypedResult Term
  --   term' sig t = fst <$> runInterp T.term (sig,()) (t,())

  --   cons :: Term -> Term -> Sort -> Term
  --   cons x xs = Cons "Cons" [x,xs]

  --   nil :: Sort -> Term
  --   nil = Cons "Nil" []

  --   some :: Term -> Sort -> Term
  --   some x = Cons "Some" [x]

  --   none :: Sort -> Term
  --   none = Cons "None" []

  --   (&) = flip ($)

  --   evalPCF module_ t = evalModule module_ (Call "eval_0_0" [] []) (t,tenv)

  --   parsePCFCaseStudy = do
  --     file <- TIO.readFile =<< getDataFileName "case-studies/pcf/pcf.aterm"
  --     case parseModule =<< parseATerm file of
  --       Left e -> fail (show e)
  --       Right module_ -> return module_

  --   zero :: Term
  --   zero = Cons "Zero" [] "Exp"

  --   succ :: Term -> Term
  --   succ e = Cons "Succ" [e] "Exp"

  --   var :: Text -> Term
  --   var x = Cons "Var" [StringLiteral x] "Exp"

  --   app :: Term -> Term -> Term
  --   app e1 e2 = Cons "App" [e1,e2] "Exp"

  --   abs :: Text -> Term -> Term -> Term
  --   abs x t e = Cons "Abs" [StringLiteral x, t, e] "Exp"

  --   num :: Term
  --   num = Cons "Num" [] "Type"

  --   tup :: [Term] -> Term
  --   tup ts = Cons "" ts (Tuple (fmap getSort ts))

  --   tenv = ConcreteTermEnv M.empty
