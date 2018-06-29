{-# LANGUAGE FlexibleContexts #-}
module Main where

import           ConcreteSemantics
import qualified Data.Abstract.HandleError         as AbstractError
import qualified Data.Abstract.Store               as AbstractStore
import qualified Data.Concrete.Error               as ConcreteError
import qualified Data.Concrete.Store               as ConcreteStore
import           Language.ECMAScript3.Lexer        (reservedOp, whiteSpace)
import           Language.ECMAScript3.Parser       (expression, parseFromString,
                                                    statement)
import           Language.ECMAScript3.Syntax       (JavaScript (..))
import           Language.LambdaJS.Desugar
import           Language.LambdaJS.ECMAEnvironment (ecma262Env)
import           Language.LambdaJS.Parser          (parseBinds)
import           Language.LambdaJS.RemoveHOAS
import           Language.LambdaJS.Syntax
import qualified Syntax                            as S
import           System.Environment
import           Text.ParserCombinators.Parsec
import           TypeSemantics

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
  OObjIterHasNext -> S.OObjIterHasNext
  OObjIterNext    -> S.OObjIterNext
  OObjIterKey     -> S.OObjIterKey

convert :: ExprPos -> S.Expr
convert expr = case expr of
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
  ECatch _ try_ catch -> S.ECatch (convert try_) (convert catch)
  EFinally _ try_ finally -> S.EFinally (convert try_) (convert finally)
  EEval _ -> S.EEval
  e -> error ("Unsupported expression: " ++ (show e))

parseEnvironment :: FilePath -> IO (ExprPos -> ExprPos)
parseEnvironment fileName = do
  src <- readFile fileName
  case parseBinds fileName src of
    Left err -> fail (show err)
    Right f  -> return f

interpConcrete :: S.Expr -> (ConcreteStore.Store S.Location Value, ConcreteError.Error String Value)
interpConcrete ast = runConcrete [] [] ast

interpType :: S.Expr -> (AbstractStore.Store S.Location S.Type', AbstractError.Error String S.Type')
interpType ast = runAbstract [] [] ast

testCase envTransformer ecmaEnv = do
  _ <- getPosition
  testStmt <- statement
  reservedOp "::"
  expectedExpr <- expression
  reservedOp ";"
  let lhs = desugarStmtsWithResult [testStmt] (\e -> envTransformer (ecmaEnv e)) (getValue (EGetField nopos (EDeref nopos $ EId nopos "$global") (EString nopos "result")))
  let rhs = desugarExpr (expectedExpr) (\e -> envTransformer (ecmaEnv (getValue e)))
  return (lhs, rhs)

testCases envTransformer ecmaEnv = do
  whiteSpace
  tests <- many (testCase envTransformer ecmaEnv)
  eof
  return tests

mainTestFile :: FilePath -> FilePath -> IO ()
mainTestFile filename envname = do
  testFile <- readFile filename
  envTransformer <- parseEnvironment envname
  case runParser (testCases envTransformer ecma262Env) [] "stdin" testFile of
    Left err    -> fail (show err)
    Right tests -> do
      let convertedCode = map (convert . removeHOAS . fst) (tests)
      let results = map (snd . interpConcrete) (convertedCode)
      let convertedShouldBe = map (convert . removeHOAS . snd) (tests)
      let shouldBe = map (snd . interpConcrete) (convertedShouldBe)
      -- Typechecking runs in an infinite loop, so it is disabled for now
      --let types = map (snd . interpType) (converted)
      putStr $ unlines (map (\(l, r) -> (if l == r then "PASS" else "FAIL") ++ ": " ++ (show l) ++ ", " ++ (show r)) (zip results shouldBe))

printTestAST :: FilePath -> IO ()
printTestAST filename = do
  testFile <- readFile filename
  case runParser (testCases id id) [] "stdin" testFile of
    Left err    -> fail (show err)
    Right tests -> do
      mapM_ (putStrLn . show . convert . removeHOAS . fst) tests

mainRunFile :: FilePath -> FilePath -> IO ()
mainRunFile filename envname = do
  str <- readFile filename

  putStrLn "Input JavaScript"
  putStrLn (show str)
  putStrLn "\n"

  case parseFromString str of
    Right (Script p script) -> do
      putStrLn "JavaScript AST"
      putStrLn (show script)
      putStrLn "\n"

      envTransformer <- parseEnvironment envname

      let converted = (convert $ removeHOAS $ desugar (Script p script) (\e -> envTransformer (ecma262Env e)))
      putStrLn "LambdaJS AST with env"
      putStrLn (show converted)
      putStrLn "\n"

      let (resEnv, val) = interpConcrete converted
      putStrLn "Environment"
      putStrLn (show resEnv)
      putStrLn "\n"

      putStrLn "Result"
      putStrLn (show val)
      putStrLn "\n"

    Left err -> fail (show err)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["file", filename, envname]     -> mainRunFile filename envname
    ["test", testlocation, envname] -> mainTestFile testlocation envname
    ["ast", testlocation]           -> printTestAST testlocation
    _                               -> fail "Invalid arguments"

