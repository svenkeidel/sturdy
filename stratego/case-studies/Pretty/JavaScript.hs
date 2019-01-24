{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pretty.JavaScript where

import Prelude hiding ((<>))

import WildcardSemantics
import Text.PrettyPrint hiding (sep)
-- import Control.Exception
-- import System.IO.Unsafe
-- import Control.DeepSeq
import qualified Data.Text as T

{-# NOINLINE tryPPJS #-}
tryPPJS :: Term -> Doc
tryPPJS t = text (show t)
  --unsafePerformIO $ catch (evaluate (force (ppJS t))) (\(msg :: ErrorCall) -> error $ "cannot pretty print term " ++ show t ++ "\nError: " ++ show msg)

ppJS :: Term -> Doc
ppJS t = case t of
  Cons "Program" [stmts] -> ppStatements stmts
  Cons "Array" _ -> ppExp t
  Cons "Call" _ -> ppExp t
  Cons "Division" _ -> ppExp t
  Cons "Substraction" _ -> ppExp t
  Cons "Addition" _ -> ppExp t
  Cons "Multiplication" _ -> ppExp t
  Cons "Cons" [x,xs] -> ppJS x $$ ppJS xs
  Cons "Nil" [] -> mempty
  Cons "" [a,b] -> parens (ppJS a <> char ',' <> ppJS b)
  Wildcard -> ppWildcard
  _ -> error $ "unexpected term while pprinting javascript: " ++ show t

ppStatements :: Term -> Doc
ppStatements = ppSepList ppStatement ($$)

ppStatement :: Term -> Doc
ppStatement t = case t of
  Cons "expStmt" [ident] -> ppIdentifier ident
  Cons "varDecl" [ident, e] -> text "var" <+> ppIdentifier ident <+> char '=' <+> ppJS e <> char ';'
  Cons "method" [ident, params, body] -> text "function" <+> ppIdentifier ident <> parens (ppParams params) <> braces (ppJS body)
  Wildcard -> ppWildcard
  _ -> error $ "unexpected term while pprinting statement: " ++ show t

ppExps :: Term -> Doc
ppExps = ppSepList' ppExp comma  

ppExp :: Term -> Doc
ppExp t = case t of
  Cons "Call" [fun,args] -> ppExp fun <> ppExps args
  Cons "Property" [e,ident] -> ppExp e <> char '.' <> ppIdentifier ident
  Cons "Identifier" [ident] -> ppIdentifier ident
  Cons "parameter" [ident] -> ppIdentifier ident
  Cons "IntegerLiteral" [lit] -> ppNumLit lit
  Cons "Addition" [e1,e2] -> ppExp e1 <+> char '+' <+> ppExp e2
  Cons "Substraction" [e1,e2] -> ppExp e1 <+> char '-' <+> ppExp e2
  Cons "Division" [e1,e2] -> ppExp e1 <+> char '/' <+> ppExp e2
  Cons "Multiplication" [e1,e2] -> ppExp e1 <+> char '*' <+> ppExp e2
  Cons "Lower" [e1,e2] -> ppExp e1 <+> char '<' <+> ppExp e2
  Cons "LowerEquals" [e1,e2] -> ppExp e1 <+> text "<=" <+> ppExp e2
  Cons "Greater" [e1,e2] -> ppExp e1 <+> char '<' <+> ppExp e2
  Cons "GreaterEquals" [e1,e2] -> ppExp e1 <+> text "<=" <+> ppExp e2
  Cons "Equals" [e1,e2] -> ppExp e1 <+> text "==" <+> ppExp e2
  Cons "NotEquals" [e1,e2] -> ppExp e1 <+> text "!=" <+> ppExp e2
  Cons "AnonFunction" [params, body] -> text "function" <> parens (ppParams params) <> braces (ppStatements body)
  Cons "Array" [e1,e2] -> ppExp e1 <> brackets (ppExp e2)
  Cons "ArrayLiteral" [e] -> brackets (ppExps e)
  Cons "String" [e] -> brackets (ppStrLit e)
  StringLiteral s -> text (T.unpack s)
  Wildcard -> ppWildcard
  _ -> error $ "unexpected term while pprinting expression: " ++ show t

ppParams :: Term -> Doc
ppParams = ppSepList' ppIdentifier comma

ppObject :: Term -> Doc
ppObject t = case t of
  Cons "KeyValue" [ident,e] -> ppIdentifier ident <> char ':' <+> ppExp e
  Cons "KeyValue" _ -> error $ "unexpected KeyValue while pprinting object"
  _ -> error $ "unexpected object while pprinting object"

ppIdentifier :: Term -> Doc
ppIdentifier = ppStrLit

ppStrLit :: Term -> Doc
ppStrLit (StringLiteral s) = text (T.unpack s)
ppStrLit Wildcard = ppWildcard
ppStrLit t = error $ "unexpected term while expecting string literal: " ++ show t

ppNumLit :: Term -> Doc
ppNumLit (NumberLiteral n) = int n
ppNumLit Wildcard = ppWildcard
ppNumLit t = error $ "unexpected term while expecting int literal: " ++ show t

ppSepList' :: (Term -> Doc) -> Doc -> Term -> Doc
ppSepList' ppElem sep = ppSepList ppElem (\d1 d2 -> d1 <> sep <> d2)

ppSepList :: (Term -> Doc) -> (Doc -> Doc -> Doc) -> Term -> Doc
ppSepList ppElem sep t = case t of
  Cons "Cons" [x, Cons "Nil" []] -> ppElem x
  Cons "Cons" [x, xs] -> ppElem x `sep` ppSepList ppElem sep xs
  Cons "Nil" [] -> mempty
  Wildcard -> ppWildcard
  _ -> error $ "unexpected term: " ++ show t


ppWildcard :: Doc
ppWildcard = char '_'
