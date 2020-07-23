{-# OPTIONS_GHC -Wno-warnings-deprecations #-}
module Parser(loadSchemeFile,loadSchemeFile') where

import           Prelude hiding (fail)

import           Control.Monad.State hiding (fail)
import           Control.Monad.Except

import           Data.Label
import           Data.Text (Text, pack)
import           Data.IORef
import qualified Data.Map.Lazy as Map

import           Language.Scheme.Types as LT hiding (body)
import           Language.Scheme.Parser
import           Language.Scheme.Core
import qualified Language.Scheme.Macro as Macro

import           Paths_sturdy_scheme

import           Syntax as S

import           Text.Printf

loadSchemeFile :: String -> IO LExpr
loadSchemeFile file = do
  contents <- readFile =<< getDataFileName (printf "scheme_files/%s" file)
  case readExprList contents of
    Left err -> throwLispError err
    Right val -> do
     expanded <- macroExpand (List val)
     print expanded
     let expr = parseTopLevelSExpr expanded
    --  print (generate expr)
     return expr

loadSchemeFile' :: String -> IO Expr
loadSchemeFile' file = do
  lexpr <- loadSchemeFile file
  return $ generate lexpr

macroExpand :: LispVal -> IO LispVal
macroExpand program = do
  env <- r7rsEnv
  removeMacros env
  addMacros env
  runErrorIO (Macro.expand env True program apply)
  where
    removeMacros :: Env -> IO ()
    removeMacros env = do
      bnds <- readIORef (bindings env)
      writeIORef (bindings env) (foldr Map.delete bnds ["m_begin","m_or","m_let","m_letrec","m_letrec*"])

    addMacros :: Env -> IO ()
    addMacros env = do
      macrosFile <- getDataFileName "scheme_files/macros.scm"
      _ <- evalString env (printf "(load \"%s\")" macrosFile)
      return ()

runErrorIO :: IOThrowsError a -> IO a
runErrorIO m = do
  e <- runExceptT m
  case e of
    Right val -> return val
    Left err  -> throwLispError err

throwLispError :: LispError -> IO a
throwLispError err = do
  str <- showLispError err
  fail str

parseTopLevelSExpr :: LispVal -> LExpr
parseTopLevelSExpr (LT.List prog) =
  let (defs,body) = parseDefinitions prog
  in let_rec defs body
parseTopLevelSExpr expr = error $ "cannot parse s-expression: " ++ show expr

parseDefinitions :: [LispVal] -> ([(Text,LExpr)],[LExpr])
parseDefinitions (LT.List [Atom "define", Atom var, body] : defs) =
  (pack var, parseSExpr body) <+ parseDefinitions defs
parseDefinitions (LT.List (Atom "define": Atom var: body) : defs) =
  (pack var, begin (map parseSExpr body)) <+ parseDefinitions defs
parseDefinitions (LT.List (Atom "define": LT.List (Atom var: args): body) : defs) =
  case args of 
    [] -> (pack var, lam [] (map parseSExpr body)) <+ parseDefinitions defs
    -- args -> (pack var, lam [ pack arg ] (map parseSExpr body)) <+ parseDefinitions defs
    _ -> (pack var, parseSExpr (LT.List (Atom "lambda": LT.List args: body))) <+ parseDefinitions defs
  -- (pack var, lam [ pack x | Atom x <- args ] (map parseSExpr body)) <+ parseDefinitions defs
parseDefinitions (sexpr : defs) =
  parseSExpr sexpr +> parseDefinitions defs
parseDefinitions [] = ([],[])

parseSExpr :: LispVal -> LExpr
parseSExpr val = case val of
  LT.Number x -> lit $ S.Int $ fromIntegral x
  LT.Float x -> lit $ S.Float x
  LT.Rational x -> lit $ S.Rational x
  LT.Bool x -> lit $ S.Bool x
  LT.Char x -> lit $ S.Char x
  LT.String x -> lit $ S.String (pack x)
  LT.Atom "#t" -> lit $ S.Bool True
  LT.Atom "#f" -> lit $ S.Bool False
  LT.Atom "null" -> list []
  LT.Atom x -> var_ (pack x)
  LT.List [Atom "define", Atom var, body] ->
    define (pack var) (parseSExpr body)
  LT.List (Atom "define": Atom var: body) ->
    define (pack var) (begin (map parseSExpr body))
  LT.List (Atom "define": LT.List (Atom var: args): body) ->
    case args of 
      [] -> define (pack var) (lam [ pack x | Atom x <- args ] (map parseSExpr body))
      _ -> define (pack var) (parseSExpr (LT.List (Atom "lambda": LT.List args: body)))
    
  -- TODO : add lambdas with variable amount of arguments if only one arguement is given
  LT.List (Atom "lambda": Atom var: body_) ->
    lam [ pack var ] (map parseSExpr body_)
  LT.List (Atom "lambda": LT.List args: body_) ->
    case args of 
      [] -> lam [] (map parseSExpr body_) 
      (Atom arg): [] -> lam [ pack arg ] (map parseSExpr body_)
      (Atom arg): args_ -> lam [ pack arg ] [parseSExpr (LT.List (Atom "lambda": LT.List args_: body_))]
    -- lam [ pack x | Atom x <- args ] (map parseSExpr body_)
  -- case lambda
  LT.List [Atom "if", cond, then_branch, else_branch] ->
    if_ (parseSExpr cond) (parseSExpr then_branch) (parseSExpr else_branch)
  LT.List [Atom "if", cond, then_branch] ->
    if_ (parseSExpr cond) (parseSExpr then_branch) (list [])
  -- begin
  LT.List (Atom "begin": body_) ->
    begin (map parseSExpr body_)
  -- begin0
  LT.List (Atom "let": LT.List params_: body_) ->
    let bnds = map extractBinding params_
    in let_ bnds (map parseSExpr body_)
  LT.List (Atom "letrec": LT.List params_: body_) ->
    let bnds = map extractBinding params_
    in let_rec bnds (map parseSExpr body_)
  LT.List [Atom "set!", Atom var, body_] ->
    set (pack var) (parseSExpr body_)
  LT.List (Atom "set!": _) -> error $ "cannot parse set!: " ++ show val
  LT.List [Atom "quote", LT.List xs] -> list (map quoteListHelp xs)
  LT.List [Atom "quote", val_] -> lit $ parseLits val_
  LT.List (LT.List(Atom "lambda": rest): args) ->
    case args of 
      [] -> app (parseSExpr (LT.List(Atom "lambda" : rest))) []
      arg: [] -> app (parseSExpr (LT.List(Atom "lambda" : rest))) [parseSExpr arg]
      args_ -> app (parseSExpr (LT.List (LT.List(Atom "lambda": rest): (init args_)))) [parseSExpr $ last args_]
    -- app (parseSExpr (LT.List(Atom "lambda" : rest))) (map parseSExpr args)
  -- LT.List [Atom "call-with-values", generator, _] ->
  --   app (parseSExpr generator) []
  LT.List [Atom "number?", e] -> op1_ IsNumber (parseSExpr e)
  LT.List [Atom "integer?", e] -> op1_ IsInteger (parseSExpr e)
  LT.List [Atom "float?", e] -> op1_ IsFloat (parseSExpr e)
  LT.List [Atom "rational?", e] -> op1_ IsRational (parseSExpr e)
  LT.List [Atom "zero?", e] -> op1_ IsZero (parseSExpr e)
  LT.List [Atom "positive?", e] -> op1_ IsPositive (parseSExpr e)
  LT.List [Atom "negative?", e] -> op1_ IsNegative (parseSExpr e)
  LT.List [Atom "even?", e] -> op1_ IsEven (parseSExpr e)
  LT.List [Atom "odd?", e] -> op1_ IsOdd (parseSExpr e)
  LT.List [Atom "boolean?", e] -> op1_ IsBoolean (parseSExpr e)
  LT.List [Atom "null?", e] -> op1_ IsNull (parseSExpr e)
  LT.List [Atom "pair?", e] -> op1_ IsCons (parseSExpr e)
  LT.List [Atom "cons?", e] -> op1_ IsCons (parseSExpr e)
  LT.List [Atom "abs", e] -> op1_ Abs (parseSExpr e)
  LT.List [Atom "floor", e] -> op1_ Floor (parseSExpr e)
  LT.List [Atom "ceiling", e] -> op1_ Ceiling (parseSExpr e)
  LT.List [Atom "log", e] -> op1_ Log (parseSExpr e)
  LT.List [Atom "not", e] -> op1_ Not (parseSExpr e)
  LT.List [Atom "car", e] -> op1list_ Car (parseSExpr e)
  LT.List [Atom "cdr", e] -> op1list_ Cdr (parseSExpr e)
  LT.List [Atom "caar", e] -> op1list_ Caar (parseSExpr e)
  LT.List [Atom "cadr", e] -> op1list_ Cadr (parseSExpr e)
  LT.List [Atom "cddr", e] -> op1list_ Cddr (parseSExpr e)
  LT.List [Atom "caddr", e] -> op1list_ Caddr (parseSExpr e)
  LT.List [Atom "cadddr", e] -> op1list_ Cadddr (parseSExpr e)
  LT.List [Atom "random", e] -> op1_ Random (parseSExpr e)
  LT.List [Atom "string->symbol", e] -> op1_ StringToSymbol (parseSExpr e)
  LT.List [Atom "symbol->string", e] -> op1_ SymbolToString (parseSExpr e)
  LT.List [Atom "number->string", e] -> op1_ NumberToString (parseSExpr e)
  -- op2
  LT.List [Atom "eq?", e1, e2] -> op2_ Eqv (parseSExpr e1) (parseSExpr e2)
  LT.List [Atom "quotient", e1, e2] -> op2_ Quotient (parseSExpr e1) (parseSExpr e2)
  LT.List [Atom "remainder", e1, e2] -> op2_ Remainder (parseSExpr e1) (parseSExpr e2)
  LT.List [Atom "modulo", e1, e2] -> op2_ Modulo (parseSExpr e1) (parseSExpr e2)
  LT.List [Atom "string-ref", e1, e2] -> op2_ StringRef (parseSExpr e1) (parseSExpr e2)
  LT.List [Atom "cons", e1, e2] -> cons (parseSExpr e1) (parseSExpr e2)
  -- opvar
  LT.List (Atom "=": args) -> opvar_ Equal (map parseSExpr args)
  LT.List (Atom "<": args) -> opvar_ Smaller (map parseSExpr args)
  LT.List (Atom ">": args) -> opvar_ Greater (map parseSExpr args)
  LT.List (Atom "<=": args) -> opvar_ SmallerEqual (map parseSExpr args)
  LT.List (Atom ">=": args) -> opvar_ GreaterEqual (map parseSExpr args)
  LT.List (Atom "max": args) -> opvar_ Max (map parseSExpr args)
  LT.List (Atom "min": args) -> opvar_ Min (map parseSExpr args)
  LT.List (Atom "+": args) -> opvar_ Add (map parseSExpr args)
  LT.List (Atom "*": args) -> opvar_ Mul (map parseSExpr args)
  LT.List (Atom "-": args) -> opvar_ Sub (map parseSExpr args)
  LT.List (Atom "/": args) -> opvar_ Div (map parseSExpr args)
  LT.List (Atom "gcd": args) -> opvar_ Gcd (map parseSExpr args)
  LT.List (Atom "lcm": args) -> opvar_ Lcm (map parseSExpr args)
  LT.List (Atom "string-append": args) -> opvar_ StringAppend (map parseSExpr args)
  LT.List (Atom "list": args) -> list (map parseSExpr args)
  LT.List [Atom "error", LT.String err] -> error_ err

  LT.List (Atom x: args) -> 
    case args of 
      [] -> app (var_ (pack x)) []
      arg: [] -> app (var_ (pack x)) [parseSExpr arg]
      args_ -> app (parseSExpr (LT.List (Atom x: (init args_)))) [parseSExpr $ last args_]
    -- app (var_ (pack x)) (map parseSExpr args)
  LT.List (fun: args) -> 
    case args of 
      [] -> app (parseSExpr fun) []
      arg: [] -> app (parseSExpr fun) [parseSExpr arg]
      args_ -> app (parseSExpr (LT.List (fun: (init args_)))) [parseSExpr $ last args_]
    -- app (parseSExpr fun) (map parseSExpr args)
  _ -> error $ "cannot parse s-expression: " ++ show val

extractBinding :: LispVal -> (Text, State Label Expr)
extractBinding (LT.List [Atom var, val]) = (pack var, parseSExpr val)
extractBinding _ = error "Error when extracting tuple"

--TODO: resolve remaining lispvals
parseLits :: LispVal -> Literal
parseLits val = case val of
  LT.Number x -> S.Int $ fromIntegral x
  LT.Float x -> S.Float x
  LT.Rational x -> S.Rational x
  LT.Bool x -> S.Bool x
  LT.Char x -> S.Char x
  LT.String x -> S.String (pack x)
  LT.Atom "#t" -> S.Bool True
  LT.Atom "#f" -> S.Bool False
  LT.Atom x -> S.Quote $ S.Symbol (pack x)
  -- LT.List xs -> list (map parseSExpr xs)
  -- LT.DottedList xs x -> S.DottedList (map parseLits xs) (parseLits x)
  _ -> error "type not supported"

quoteListHelp :: LispVal -> LExpr
quoteListHelp val = case val of
  LT.Number x -> lit $ S.Int $ fromIntegral x
  LT.Float x -> lit $ S.Float x
  LT.Rational x -> lit $ S.Rational x
  LT.Bool x -> lit $ S.Bool x
  LT.Char x -> lit $ S.Char x
  LT.String x -> lit $ S.String (pack x)
  LT.Atom "#t" -> lit $ S.Bool True
  LT.Atom "#f" -> lit $ S.Bool False
  LT.Atom x -> lit $ S.Quote $ S.Symbol (pack x)
  LT.List xs -> list (map quoteListHelp xs)
  -- LT.DottedList xs x -> S.DottedList (map parseLits xs) (parseLits x)
  _ -> error "type not supported"

(<+) :: a -> ([a],[b]) -> ([a],[b])
a1 <+ (a2,b2) = (a1 : a2, b2)

(+>) :: b -> ([a],[b]) -> ([a],[b])
b1 +> (a2,b2) = (a2, b1 : b2)
