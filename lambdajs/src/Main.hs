{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Language.ECMAScript3.Lexer        (reservedOp, whiteSpace)
import           Language.ECMAScript3.Parser       (parseBlockStmt,
                                                    parseExpression,
                                                    parseJavaScriptFromFile,
                                                    parseScriptFromString)
import           Language.ECMAScript3.Syntax       (JavaScript (..))
import           Language.LambdaJS.Desugar
import           Language.LambdaJS.ECMAEnvironment (ecma262Env)
import           Language.LambdaJS.Parser          (parseBinds)
import           Language.LambdaJS.PrettyPrint
import           Language.LambdaJS.RemoveHOAS
import           Language.LambdaJS.Syntax
import           SharedConcrete
import qualified Syntax                            as S
import           System.Environment
import           System.IO
import           Text.ParserCombinators.Parsec
import           Text.PrettyPrint.HughesPJ

convertOp :: Op -> S.Op
convertOp o = case o of
  ONumPlus        -> S.ONumPlus
  OStrPlus        -> S.OStrPlus
  OMul            -> S.OMul
  ODiv            -> S.ODiv
  OMod            -> S.OMod
  OSub            -> S.OSub
  OLt             -> S.OLt
  OStrLt          -> S.OStrLt
  OBAnd           -> S.OBAnd
  OBOr            -> S.OBOr
  OBXOr           -> S.OBXOr
  OBNot           -> S.OBNot
  OLShift         -> S.OLShift
  OSpRShift       -> S.OSpRShift
  OZfRShift       -> S.OZfRShift
  OStrictEq       -> S.OStrictEq
  OAbstractEq     -> S.OAbstractEq
  OTypeof         -> S.OTypeof
  OPrimToNum      -> S.OPrimToNum
  OPrimToStr      -> S.OPrimToStr
  OPrimToBool     -> S.OPrimToBool
  OIsPrim         -> S.OIsPrim
  OHasOwnProp     -> S.OHasOwnProp
  OToInteger      -> S.OToInteger
  OToInt32        -> S.OToInt32
  OToUInt32       -> S.OToUInt32
  OStrStartsWith  -> S.OStrStartsWith
  OStrLen         -> S.OStrLen
  OMathExp        -> S.OMathExp
  OMathLog        -> S.OMathLog
  OMathCos        -> S.OMathCos
  OMathSin        -> S.OMathSin
  OMathAbs        -> S.OMathAbs
  OMathPow        -> S.OMathPow
  OSurfaceTypeof  -> S.OSurfaceTypeof
  OObjCanDelete   -> S.OObjCanDelete
  ORegExpMatch    -> S.ORegExpMatch
  ORegExpQuote    -> S.ORegExpQuote
  OStrContains    -> S.OStrContains
  OStrSplitRegExp -> S.OStrSplitRegExp
  OStrSplitStrExp -> S.OStrSplitStrExp
  OPrint          -> S.OPrint
  _               -> error ("Unsupported operation: " ++ (show o))

convert :: ExprPos -> S.Expr
convert exp = case exp of
  ENumber _ d -> S.ENumber d
  EString _ s -> S.EString s
  EBool _ b -> S.EBool b
  EUndefined _ -> S.EUndefined
  ENull _ -> S.ENull
  ELambda _ ids body -> S.ELambda ids (convert body)
  EObject _ fields -> S.EObject (map (\(n, e) -> (n, convert e)) fields)
  EId _ ident -> S.EId ident
  EOp _ op exps -> S.EOp (convertOp op) (map convert exps)
  EApp _ body args -> S.EApp (convert body) (map convert args)
  ELet _ obj body -> S.ELet (map (\(n, e) -> (n, convert e)) obj) (convert body)
  ESetRef _ ref val -> S.ESetRef (convert ref) (convert val)
  ERef _ val -> S.ERef (convert val)
  EDeref _ ref -> S.EDeref (convert ref)
  EGetField _ obj field -> S.EGetField (convert obj) (convert field)
  EUpdateField _ obj field val -> S.EUpdateField (convert obj) (convert field) (convert val)
  EDeleteField _ obj field -> S.EDeleteField (convert obj) (convert field)
  ESeq _ one two -> S.ESeq (convert one) (convert two)
  EIf _ cond then_ else_ -> S.EIf (convert cond) (convert then_) (convert else_)
  EWhile _ cond body -> S.EWhile (convert cond) (convert body)
  ELabel _ l body -> S.ELabel (S.Label l) (convert body)
  EBreak _ l body -> S.EBreak (S.Label l) (convert body)
  EThrow _ val -> S.EThrow (convert val)
  ECatch _ try catch -> S.ECatch (convert try) (convert catch)
  EFinally _ try finally -> S.EFinally (convert try) (convert finally)
  EEval e -> S.EEval
  _ -> error ("Unsupported expression: " ++ (show exp))

parseEnvironment fileName = do
  src <- readFile fileName
  case parseBinds fileName src of
    Left err -> fail (show err)
    Right f  -> return f

interp ast = runConcrete [] [] ast

testCase envTransformer = do
  srcLoc <- getPosition
  testStmt <- parseBlockStmt
  reservedOp "::"
  expectedExpr <- parseExpression
  reservedOp ";"
  --let src = renderExpr (EString nopos $ show srcLoc)
  return $ desugarStmtsWithResult [testStmt] (\e -> envTransformer (ecma262Env e))
              (getValue (EGetField nopos (EDeref nopos $ EId nopos "$global") (EString nopos "result")))
  --let rhs = getValue $ desugarExpr expectedExpr ecma262Env

testCases envTransformer = do
  whiteSpace
  tests <- many (testCase envTransformer)
  eof
  return tests

mainTestFile filename envname = do
  testFile <- readFile filename
  envTransformer <- parseEnvironment envname
  case runParser (testCases envTransformer) [] "stdin" testFile of
    Left err    -> fail (show err)
    Right tests -> mapM_ (putStrLn . show . snd) (map (\test -> interp $ convert $ removeHOAS $ test) (take 1 $ tests))

mainRunFile filename envname = do
  str <- readFile filename

  putStrLn "Input JavaScript"
  putStrLn (show str)
  putStrLn "\n"

  case parseScriptFromString "<stdin>" str of
    Right (Script p script) -> do
      putStrLn "JavaScript AST"
      putStrLn (show script)
      putStrLn "\n"

      envTransformer <- parseEnvironment envname

      let converted = (convert $ removeHOAS $ desugar (Script p script) (\e -> envTransformer (ecma262Env e)))
      putStrLn "LambdaJS AST with env"
      putStrLn (show converted)
      putStrLn "\n"

      let (resEnv, val) = interp converted
      putStrLn "Environment"
      putStrLn (show resEnv)
      putStrLn "\n"

      putStrLn "Result"
      putStrLn (show val)
      putStrLn "\n"

    Left err -> fail (show err)

main = do
  args <- getArgs
  case args of
    ["file", filename, envname]     -> mainRunFile filename envname
    ["test", testlocation, envname] -> mainTestFile testlocation envname
    _                               -> fail "Invalid arguments"

