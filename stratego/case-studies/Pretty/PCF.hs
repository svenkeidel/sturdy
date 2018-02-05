{-# LANGUAGE OverloadedStrings #-}
module Pretty.PCF where

import WildcardSemantics
import Text.PrettyPrint

ppPCF :: Term -> Doc
ppPCF t = case t of
  Cons "Var" [v] -> text (show v)
  Cons "App" [e1,e2] ->
    ppPCF e1 <> space <> ppPCF e2
  Cons "Abs" [x,ty,e] ->
    char '\\' <> text (show x) <> char ':' <> ppType ty <> text ". " <> ppPCF e
  Cons "Zero" [] -> char '0'
  Cons "Succ" [e] -> text "succ" <> parens (ppPCF e)
  Cons "Add" [e1, e2] -> parens (ppPCF e1 <> text "+" <> ppPCF e2)
  Cons "Mul" [e1, e2] -> parens (ppPCF e1 <> text "*" <> ppPCF e2)
  Cons "Pred" [e] -> text "pred" <> parens (ppPCF e)
  Cons "Ifz" [e1,e2,e3] -> parens $ text "if " <> ppPCF e1 <> text " then " <> ppPCF e2 <> text " else " <> ppPCF e3
  Wildcard -> ppWildcard
  _ -> error $ "unexpected term: " ++ show t

ppType :: Term -> Doc
ppType t = case t of
  Cons "Num" [] -> text "num"
  Cons "Fun" [e1,e2] -> parens (ppType e1 <> text " -> " <> ppType e2)
  Wildcard -> ppWildcard
  _ -> error $ "unexpected term: " ++ show t

ppWildcard :: Doc
ppWildcard = char '?'
