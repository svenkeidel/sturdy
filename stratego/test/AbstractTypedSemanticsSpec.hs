{-# LANGUAGE FlexibleContexts #-}
module AbstractTypedSemanticsSpec where

-- import           AbstractTypedSemantics
-- import           Sort
-- import           Signature (Signature)
-- import qualified Signature as S
-- import           InterpreterArrow
-- import           Paths_sturdy_stratego
-- import           Syntax(parseModule,Strat(..))

-- import           Data.ATerm(parseATerm)
-- import           Data.PowersetResult
-- import           Data.Result
-- import qualified Data.Term as T
-- import           Data.TermEnv
-- import qualified Data.Text.IO as TIO
-- import           Data.Foldable
-- import           Data.List

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
  --   term' S.empty (T.Cons "Nil" []) `shouldMatchList` [Success (nil (List Bottom))]

  --   term' S.empty (T.Cons "Cons" [StringLiteral "foo", StringLiteral "bar"])
  --     `shouldMatchList` []

  --   term' S.empty (T.Cons "Cons" [StringLiteral "foo", nil (List Bottom)])
  --     `shouldMatchList` [Success (cons (StringLiteral "foo") (nil (List "String")) (List "String"))]

  --   term' sig (T.Cons "Cons" [StringLiteral "foo", nil (List Bottom)])
  --     `shouldMatchList` [Success (cons (StringLiteral "foo") (nil (List "String")) (List "String"))]

  --   let t = List (Coproduct "INT" "String")
  --     in term' sig (T.Cons "Cons" [NumberLiteral 1, cons (StringLiteral "foo") (nil (List "String")) (List "String")]) `shouldBe`
  --          [Success (cons (NumberLiteral 1) (cons (StringLiteral "foo") (nil t) t) t)]

  --   term' sig (T.Cons "Cons" [Wildcard Top, Wildcard Top])
  --     `shouldMatchList` [Success (cons (Wildcard Top) (Wildcard (List Top)) (List Top))]

  --   term' sig (T.Cons "Cons" [Wildcard "String", Wildcard Top])
  --     `shouldMatchList` [Success (cons (Wildcard "String") (Wildcard (List "String")) (List "String"))]

  --   term' sig (T.Cons "Cons" [Wildcard Top, Wildcard (List "String")])
  --     `shouldMatchList` [Success (cons (Wildcard Top) (Wildcard (List Top)) (List Top))]

  --   term' sig (T.Cons "Cons" [Wildcard Top, nil (List Bottom)])
  --     `shouldMatchList` [Success (cons (Wildcard Top) (nil (List Top)) (List Top))]

  --   term' sig (T.Cons "Cons" [NumberLiteral 1, cons (Wildcard Top) (nil (List Top)) (List Top)])
  --     `shouldMatchList` [Success (cons (NumberLiteral 1) (cons (Wildcard Top) (nil (List Top)) (List Top)) (List Top))]

  --   term' sig (T.Cons "Cons" [some (Wildcard Top) (Option Top), nil (List Bottom)])
  --     `shouldMatchList` [Success (cons (some (Wildcard Top) (Option Top)) (nil (List (Option Top))) (List (Option Top)))]

  --   term' sig (T.Cons "Add" [Wildcard Top, Wildcard Top])
  --     `shouldMatchList` [Success (Cons "Add" [Wildcard "Expr", Wildcard "Expr"] "Expr")]

  --   term' sig (T.Cons "Add" [NumberLiteral 1, Wildcard Top])
  --     `shouldMatchList` [Success (Cons "Add" [NumberLiteral 1, Wildcard "Expr"] "Expr")]

  --   term' sig (T.Cons "Add" [StringLiteral "foo", Wildcard Top])
  --     `shouldMatchList` [Success (Cons "Add" [StringLiteral "foo", Wildcard "Expr"] "Expr")]


  -- it "should refine wildcards on matching" $ do
  --   matchTerm' sig (Wildcard (Tuple [List (Tuple ["String","Expr"]), "Expr"]))
  --     `shouldMatchList` [Success (T.Cons "" [Wildcard (List (Tuple ["String", "Expr"])), Wildcard "Expr"])]

  --   matchTerm' sig (Wildcard (List (Tuple ["String","Expr"])))
  --     `shouldMatchList` [Success (T.Cons "Nil" []),
  --                        Success (T.Cons "Cons" [Wildcard (Tuple ["String", "Expr"]),
  --                                                Wildcard (List (Tuple ["String", "Expr"]))])]

  --   nub (matchTerm' sig (Wildcard "Expr"))
  --     `shouldMatchList` [Success (T.Cons "Add" [Wildcard "Expr", Wildcard "Expr"]), Success T.Wildcard]

  -- describe "Case Studies" $ describe "PCF" $ beforeAll parsePCFCaseStudy $
  --   it "should execute well typed programs without a problem" $ \m ->
  --     evalPCF 3 m (Wildcard (Tuple [List (Tuple ["String","Exp"]), "Exp"]))
  --       `shouldSatisfy` not . null

  -- where 
  --   term' :: Signature -> T.TermF Term -> [Result Term]
  --   term' sig t = fmap fst <$> toList (unPowRes (runInterp T.term (sig,()) (t,())))

  --   matchTerm' :: Signature -> Term -> [Result (T.TermF Term)]
  --   matchTerm' sig t = fmap fst <$> toList (unPowRes (runInterp T.matchTerm (sig,()) (t,())))

  --   cons :: Term -> Term -> Sort -> Term
  --   cons x xs = Cons "Cons" [x,xs]

  --   some :: Term -> Sort -> Term
  --   some x = Cons "Some" [x]

  --   nil :: Sort -> Term
  --   nil = Cons "Nil" []

  --   (&) = flip ($)

  --   evalPCF n module_ t = toList $ unPowRes $ evalModule n module_ (Call "eval_0_0" [] []) (t,abstractTermEnv [])

  --   parsePCFCaseStudy = do
  --     file <- TIO.readFile =<< getDataFileName "case-studies/pcf/pcf.aterm"
  --     case parseModule =<< parseATerm file of
  --       Left e -> fail (show e)
  --       Right module_ -> return module_
