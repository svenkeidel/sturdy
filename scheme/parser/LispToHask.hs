module LispToHask where

import Prelude hiding (fail)
import LispTypes as LT
import Syntax as S
import Data.Label
import Data.Text (Text, pack)
import Data.Either

import Control.Monad.State hiding (fail)


getTopDefinesLam :: [LispVal] -> [(Text, State Label Expr)]
getTopDefinesLam [] = []
getTopDefinesLam (x:xs) = case x of
  LT.List [Atom "define-values", LT.List[Atom var], body_] ->
    (pack var, lispToExpr body_) : (getTopDefinesLam xs)
  _ -> getTopDefinesLam xs

getBody :: [LispVal] -> [State Label Expr]
getBody [] = []
getBody (x:xs) = case x of
  LT.List [Atom "define-values", LT.List[Atom _], _] ->
    getBody xs
  _ -> (lispToExpr x) : (getBody xs)

match :: [LispVal] -> Either String [LispVal]
match [LT.List [Atom "module", _, _, LT.List(Atom "#%module-begin": LT.List (Atom "module": _): program)]] = Right program
match xs = Left ("unknown program structure" ++ show xs )

--TODO: should throw error instead of String "undefined" ..
lispToExpr :: LispVal -> State Label Expr
lispToExpr val = case val of
  LT.Number x -> lit $ S.Number $ fromIntegral x
  LT.Float x -> lit $ S.Float x
  LT.Ratio x -> lit $ S.Ratio x
  LT.Bool x -> lit $ S.Bool x
  LT.Character x -> lit $ S.Char x
  LT.String x -> lit $ S.String x
  LT.Atom "#t" -> lit $ S.Bool True
  LT.Atom "#f" -> lit $ S.Bool False
  LT.Atom "null" -> list []
  LT.Atom x -> var_ (pack x)
  LT.List [Atom "define-values", LT.List[Atom var], form] ->
    define (pack var) (lispToExpr form)
  -- TODO : add lambdas with variable amount of arguments if only one arguement is given
  LT.List (Atom "lambda": Atom var: body_) ->
    lam [(pack var)] (map lispToExpr body_)
  LT.List (Atom "lambda": LT.List params_: body_) -> do
    let res = rights $ map extractVar params_
    if length res == length params_
      then lam res (map lispToExpr body_)
      else error "wrong format for params in lambda"
  -- case lambda
  LT.List [Atom "if", cond, then_branch, else_branch] ->
    if_ (lispToExpr cond) (lispToExpr then_branch) (lispToExpr else_branch)
  -- begin
  LT.List (Atom "begin": body_) ->
    begin (map lispToExpr body_)
  -- begin0
  -- TODO correct error catching
  LT.List (Atom "let-values": LT.List params_: body_) -> do
    let res = rights $ map extractTuple params_
    if length res == length params_
      then let_ res (map lispToExpr body_)
      else error "wrong format for params in let"
  -- TODO correct error catching
  LT.List (Atom "letrec-values": LT.List params_: body_) -> do
    let res = rights $ map extractTuple params_
    if length res == length params_
      then let_rec res (map lispToExpr body_)
      else error "wrong format for params in letrec"
  LT.List [Atom "set!", Atom var, body_] ->
    set (pack var) (lispToExpr body_)
  LT.List [Atom "quote", LT.List xs] -> list (map quoteListHelp xs)
  LT.List [Atom "quote", val_] -> lit $ lispToLits val_

  -- quote-syntax
  -- quote-syntax
  -- with-continuation-mark
  LT.List (Atom "#%app": LT.List(Atom "lambda": rest): args) ->
    app (lispToExpr (LT.List(Atom "lambda" : rest))) (map lispToExpr args)
  LT.List [Atom "#%app", Atom "call-with-values", generator, _] ->
    app (lispToExpr generator) []
  LT.List (Atom "#%app": Atom x: args) -> case x of
    -- op1
    "number?" -> op1_ Number_ (lispToExpr $ head args)
    "integer?" -> op1_ Integer_ (lispToExpr $ head args)
    "float?" -> op1_ Float_ (lispToExpr $ head args)
    "rational?" -> op1_ Ratio_ (lispToExpr $ head args)
    "zero?" -> op1_ Zero (lispToExpr $ head args)
    "positive?" -> op1_ Positive (lispToExpr $ head args)
    "negative?" -> op1_ Negative (lispToExpr $ head args)
    "odd?" -> op1_ Odd (lispToExpr $ head args)
    "even?" -> op1_ Even (lispToExpr $ head args)
    "abs" -> op1_ Abs (lispToExpr $ head args)
    "floor" -> op1_ Floor (lispToExpr $ head args)
    "ceiling" -> op1_ Ceiling (lispToExpr $ head args)
    "log" -> op1_ Log (lispToExpr $ head args)
    "boolean?" -> op1_ Boolean (lispToExpr $ head args)
    "not" -> op1_ Not (lispToExpr $ head args)
    "null?" -> op1_ Null (lispToExpr $ head args)
    "list?" -> op1_ ListS (lispToExpr $ head args)
    "pair?" -> op1_ ConsS (lispToExpr $ head args)
    "cons?" -> op1_ ConsS (lispToExpr $ head args)
    "car" -> op1_ Car (lispToExpr $ head args)
    "cdr" -> op1_ Cdr (lispToExpr $ head args)
    "caar" -> op1_ Caar (lispToExpr $ head args)
    "cadr" -> op1_ Cadr (lispToExpr $ head args)
    "cddr" -> op1_ Cddr (lispToExpr $ head args)
    "caddr" -> op1_ Caddr (lispToExpr $ head args)
    "error" -> op1_ Error (lispToExpr $ head args)
    -- op2
    "eq?" -> op2_ Eqv (lispToExpr $ head args) (lispToExpr $ last args)
    -- "equal?" -> op2_ Equal (lispToExpr $ head args) (lispToExpr $ last args)
    "quotient" -> op2_ Quotient (lispToExpr $ head args) (lispToExpr $ last args)
    "remainder" -> op2_ Remainder (lispToExpr $ head args) (lispToExpr $ last args)
    "modulo" -> op2_ Modulo (lispToExpr $ head args) (lispToExpr $ last args)
    "cons" -> cons (lispToExpr $ head args) (lispToExpr $ last args)
    -- "assq" -> op2_ Assq (lispToExpr $ head args) (lispToExpr $ last args)
    -- opvar
    "=" -> opvar_ EqualS (map lispToExpr args)
    "<" -> opvar_ SmallerS (map lispToExpr args)
    ">" -> opvar_ GreaterS (map lispToExpr args)
    "<=" -> opvar_ SmallerEqualS (map lispToExpr args)
    ">=" -> opvar_ GreaterEqualS (map lispToExpr args)
    "max" -> opvar_ Max (map lispToExpr args)
    "min" -> opvar_ Min (map lispToExpr args)
    "+" -> opvar_ Add (map lispToExpr args)
    "*" -> opvar_ Mul (map lispToExpr args)
    "-" -> opvar_ Sub (map lispToExpr args)
    "/" -> opvar_ Div (map lispToExpr args)
    "gcd" -> opvar_ Gcd (map lispToExpr args)
    "lcm" -> opvar_ Lcm (map lispToExpr args)
    -- "and" -> opvar_ And (map lispToExpr args)
    -- "or" -> opvar_ Or (map lispToExpr args)
    "list" -> list (map lispToExpr args)
    _ -> app (var_ (pack x)) (map lispToExpr args)
  LT.List (Atom "#%app": foo: args) ->
    app (lispToExpr foo) (map lispToExpr args)
  _ -> lit $ S.Quote $ S.String ("undefined " ++ show val)

extractTuple :: LispVal -> Either String (Text, State Label Expr)
extractTuple (LT.List [LT.List [Atom var], val]) = Right (pack var, lispToExpr val)
extractTuple _ = Left "Error when extracting tuple"

extractVar :: LispVal -> Either String Text
extractVar (Atom x) = Right $ pack x
extractVar _ = Left "Error when extracting var"

--TODO: resolve remaining lispvals
lispToLits :: LispVal -> Literal
lispToLits val = case val of
  LT.Number x -> S.Number $ fromIntegral x
  LT.Float x -> S.Float x
  LT.Ratio x -> S.Ratio x
  LT.Bool x -> S.Bool x
  LT.Character x -> S.Char x
  LT.String x -> S.String x
  LT.Atom "#t" -> S.Bool True
  LT.Atom "#f" -> S.Bool False
  LT.Atom x -> S.Quote $ S.Symbol x
  -- LT.List xs -> list (map lispToExpr xs)
  -- LT.DottedList xs x -> S.DottedList (map lispToLits xs) (lispToLits x)
  _ -> error "type not supported"

quoteListHelp :: LispVal -> State Label Expr   
quoteListHelp val = case val of
  LT.Number x -> lit $ S.Number $ fromIntegral x
  LT.Float x -> lit $ S.Float x
  LT.Ratio x -> lit $ S.Ratio x
  LT.Bool x -> lit $ S.Bool x
  LT.Character x -> lit $ S.Char x
  LT.String x -> lit $ S.String x
  LT.Atom "#t" -> lit $ S.Bool True
  LT.Atom "#f" -> lit $ S.Bool False
  LT.Atom x -> lit $ S.Quote $ S.Symbol x
  LT.List xs -> list (map quoteListHelp xs)
  -- LT.DottedList xs x -> S.DottedList (map lispToLits xs) (lispToLits x)
  _ -> error "type not supported"
