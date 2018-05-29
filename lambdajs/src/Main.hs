module Main where

import Language.LambdaJS.Desugar
import Language.LambdaJS.Syntax
import Language.LambdaJS.RemoveHOAS
import qualified Syntax as S
import Language.ECMAScript3.Syntax (JavaScript (..))
import Language.ECMAScript3.Parser (parseScriptFromString, parseBlockStmt, 
  parseExpression, parseJavaScriptFromFile)
import Language.LambdaJS.ECMAEnvironment (ecma262Env)
import Text.ParserCombinators.Parsec
import Language.ECMAScript3.Lexer (reservedOp, whiteSpace)
import System.Environment
import System.IO
import SharedConcrete

convertOp :: Op -> S.Op
convertOp o = case o of
  ONumPlus -> S.ONumPlus
  OStrPlus -> S.OStrPlus
  OMul -> S.OMul
  ODiv -> S.ODiv
  OMod -> S.OMod
  OSub -> S.OSub
  OLt -> S.OLt
  OStrLt -> S.OStrLt
  OBAnd -> S.OBAnd
  OBOr -> S.OBOr
  OBXOr -> S.OBXOr
  OBNot -> S.OBNot
  OLShift -> S.OLShift
  OSpRShift -> S.OSpRShift
  OZfRShift -> S.OZfRShift
  OStrictEq -> S.OStrictEq
  OAbstractEq -> S.OAbstractEq
  OTypeof -> S.OTypeof
  OPrimToNum -> S.OPrimToNum
  OPrimToStr -> S.OPrimToStr
  OPrimToBool -> S.OPrimToBool
  OIsPrim -> S.OIsPrim
  OHasOwnProp -> S.OHasOwnProp
  OToInteger -> S.OToInteger
  OToInt32 -> S.OToInt32
  OToUInt32 -> S.OToUInt32
  OStrStartsWith -> S.OStrStartsWith
  OStrLen -> S.OStrLen
  OMathExp -> S.OMathExp
  OMathLog -> S.OMathLog
  OMathCos -> S.OMathCos
  OMathSin -> S.OMathSin
  OMathAbs -> S.OMathAbs
  OMathPow -> S.OMathPow
  OSurfaceTypeof -> S.OSurfaceTypeof
  OObjCanDelete -> S.OObjCanDelete
  ORegExpMatch -> S.ORegExpMatch
  ORegExpQuote -> S.ORegExpQuote
  OStrContains -> S.OStrContains
  OStrSplitRegExp -> S.OStrSplitRegExp
  OStrSplitStrExp -> S.OStrSplitStrExp
  OPrint -> S.OPrint
  _ -> error ("Unsupported operation: " ++ (show o))

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

main = do
  [filename] <- getArgs
  str <- readFile filename
  putStrLn $ "Input JavaScript"
  putStrLn $ "  " ++ (show str)

  case parseScriptFromString "<stdin>" str of
    Right (Script p script) -> do
      putStrLn $ "JavaScript AST"
      putStrLn $ "  " ++ (show script)

      let converted = (convert $ removeHOAS $ desugar (Script p script) id)
      putStrLn $ "LambdaJS AST"
      putStrLn $ "  " ++ (show converted)

      let (env, val) = runConcrete [("$global", S.Location (-1))] [(S.Location (-1), VObject [])] converted
      putStrLn $ "Environment" 
      putStrLn $ "  " ++ (show env)
      putStrLn $ "Result"
      putStrLn $ "  " ++ (show val)
    Left err -> do
      putStrLn (show err)
